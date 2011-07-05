{-# LANGUAGE BangPatterns, RankNTypes #-}
import qualified Text.Parsec.Token as T
import Text.Parsec.Combinator
{--
import Text.Parsec.Char
import Text.Parsec.String
--}
import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Language
import Data.Char
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Data.Vector hiding (forM_, foldl', (!), map, length, (++), mapM, mapM_, modify, replicateM, last, zipWith)
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Data.Vector.Unboxed.Mutable hiding (length, read)
import Data.Map hiding (map)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.Set as S hiding (map)
import Data.List
import Control.Monad
import Control.Exception
import SVM
import System
import Debug.Trace
import System.IO
import Control.Monad.ST
import Control.Monad.State

type User = Int
type Item = Int
type Rating = Double

type RTuple = (User, Item, Rating)

type UserRating = M.Map Item Rating
type ItemRating = M.Map User Rating

type Vec = V.Vector

data Kernel = Kernel {ukf :: UV.Vector Double, ikf :: UV.Vector Double}
type KS = State Kernel
--natural = T.natural haskell
--whiteSpace = T.whiteSpace haskell
natural :: Parser Int
natural = fmap (post 0) $ many1 digit
  where
    post ac []     = (ac * 10) 
    post ac [x]    = (ac * 10) + digitToInt x
    post ac (x:xs) = post (ac * 10 + digitToInt x) xs
whiteSpace = many (oneOf " \t") >> return ()

tuples = sepEndBy tuple (char '\n')
--tuples = many tuple
tuple = do
    u <- natural
    whiteSpace
    i <- natural
    whiteSpace
    r <- natural
    whiteSpace
    t <- many1 digit
    return (u, i, fromIntegral r::Double)
    --return (fromIntegral u::Int, fromIntegral i::Int, fromIntegral r::Double)

tuplesFromFile :: FilePath -> IO [RTuple]
tuplesFromFile file = do
    res <- parseFromFile tuples file
    case res of
        Left e -> error $ show e
        Right ts -> return ts

mlconvert = Main.convert (943, 1682)
convert :: (Int, Int) -> [RTuple] -> IO (V.Vector UserRating, V.Vector ItemRating)
convert dim ts = construct where
    (nu, ni) = foldl' maxTuple dim ts
    maxTuple :: (Int, Int) -> RTuple -> (Int, Int)
    maxTuple (a, b) (i, j, _) = (max a i, max b j)

    construct :: IO (V.Vector UserRating, V.Vector ItemRating)
    construct = do
        urs <- GM.replicate (nu + 1) M.empty
        irs <- GM.replicate (ni + 1) M.empty
        --urs :: MV.MVector m UserRating
        --irs :: MV.MVector m ItemRating
        forM_ ts $ \ (u, i, r) -> do
            ur <- MV.read urs u
            let ur' = M.insert i r ur
            MV.write urs u ur'

            ir <- MV.read irs i
            let ir' = M.insert u r ir
            MV.write irs i ir'
        urs' <- G.unsafeFreeze urs
        irs' <- G.unsafeFreeze irs
        return (urs', irs')

{--
instance HasSize Map where
    size = M.size
--}
--statistics :: Vec Map k v ->  Vec Map 
avg :: (Fractional v) => Map k v -> v
avg m = M.fold (+) 0 m / fromIntegral (M.size m)

--gavg :: (Fractional v) => Vec (Map k v) -> v
gavg v = go 1 0 0 where
    go :: Int -> Double -> Int -> Double
    go i sum s
        | i < V.length v = let m = v V.! i in go (i+1) (sum + M.fold (+) 0 m) (s + M.size m)
        | otherwise = sum / fromIntegral s

similarity :: Vec (Map Int Double) -> Int -> Int -> Double
similarity v i j = go (S.elems commonKeySet) 0 0 0 where
    mi = v V.! i
    mj = v V.! j
    commonKeySet = S.intersection (keysSet mi)  (keysSet mj)
    ncks = fromIntegral $ S.size commonKeySet
    avgs = V.map avg v -- TODO: optimize this
    go [] ac1 ac2 ac3
        | ac2 == 0 || ac3 == 0 = 0
        | otherwise = ac1 / (sqrt ac2 * sqrt ac3)-- * (ncks / ncks + 100)
    go (ck:cks) ac1 ac2 ac3 = go cks ac1' ac2' ac3' where
        ac1' = ac1 + (mi ! ck - avgs V.! i) * (mj ! ck - avgs V.! j)
        ac2' = ac2 + (mi ! ck - avgs V.! i) ^ 2
        ac3' = ac3 + (mj ! ck - avgs V.! j) ^ 2

fit :: Double -> Bool
fit r | r <= 5 && r >= 1 = True
      | otherwise = False

filterIndex :: (a -> Bool) -> [a] -> [(Int, a)]
filterIndex _ [] = []
filterIndex pred as = go pred as 1 where
    go _ [] _ = []
    go pred (a:as) index =
        let rest = go pred as (index+1) in
            if pred a then ((index,a) : rest) else rest

filterIndex' :: (a -> Bool) -> [a] -> [Int]
--filterIndex' _ [] = []
filterIndex' pred as = go pred as 1 where
    go _ [] _ = []
    go pred (a:as) index = 
        let rest = go pred as (index+1) in
            if pred a then (index : rest) else rest
--divergentIndex :: [a] -> [a] -> [Int]
--divergentIndex [] _ = []
--divergentIndex _ [] = []
--divergentIndex (a:as) (b:bs) = go pred as bs 1 where
--    go 
limit min max a
    | a < min = min
    | a > max = max
    | otherwise = a

run :: Vec UserRating -> Vec ItemRating -> Double -> Int -> Double -> Int -> [RTuple] -> [[(Double, Double, Double)]]
run urs irs eps iterNum step gen rts = evalState (replicateM gen ps) initState where
    ps = do
        usvrs <- computeUsvrs
        isvrs <- computeIsvrs
        mapM (mypredict usvrs isvrs) rts
    mypredict usvrs isvrs (u,i,r) = do
        usvm <- computeUsvm
        isvm <- computeIsvm
        let up = case usvrs V.! u of
                    Just sol -> predict isvm sol i
                    Nothing -> gavg urs
            ip = case isvrs V.! i of
                    Just sol -> predict usvm sol u
                    Nothing -> gavg urs
            p | fit up && fit ip = if abs (up - ua) > abs (ip - ia) then ip else up
              | fit up = up
              | fit ip = ip
              | otherwise = (ia + ua) / 2
        return (up, ip, p) where
            ua = uavgs V.! u
            ia = iavgs V.! i

    computeUsvrs = V.mapM trainUser urs
    computeIsvrs = V.mapM trainItem irs

    computeUsvm :: KS (LSSVM Int)
    computeUsvm = do
        ukfCache <- gets ukf
        let kf _ ua ub = ukfCache UV.! (pos ua ub)
        return $ LSSVM (KernelFunction kf) cost params

    computeIsvm :: KS (LSSVM Int)
    computeIsvm = do
        ikfCache <- gets ikf
        let kf _ ia ib = ikfCache UV.! (pos ia ib)
        return $ LSSVM (KernelFunction kf) cost params
    
    uavgs = V.map avg urs
    iavgs = V.map avg irs
--    go :: forall k v. KS (LSSVM Int) -> M.Map k v -> KS (Maybe (SVMSolution Int))
    --trainUser = train computeUsvm ukf
    --trainItem = train computeIsvm ikf
    trainUser r
        | M.size r > 1 = do
            svm <- computeIsvm
            km <- gets ikf-- get kernel matrix vector
            let sol = solve svm dataset eps iterNum
                ps =  simulate svm sol points
                outliers = filterIndex' (>1) $ zipWith ((abs .).(-)) ps (M.elems r)
                gradient = do
                    i <- [2..M.size r]
                    --j <- [1..i-1]
                    j <- outliers
                    --(j, ej) <- diff ps values
                    guard (i /= j)
                    let alp = alpha sol
                        --val = alp UA.! i * ej -- * values UA.! i
                        val = alp UA.! i * alp UA.! j-- * values UA.! i * values UA.! j
                    return (pos (points UA.! i) (points UA.! j), step * val)
                --update :: ST s ()
                update v (p,g) = UMV.read v p >>= \val -> write v p $lm (val + g)
                updateAll v = mapM_ (update v) gradient
                nkm = UV.modify updateAll km
            modify (\k -> k{ikf = nkm})
            return $ Just sol
        | otherwise = return Nothing
            where
                dataset = DataSet points values
                points = A.listArray (1, M.size r) $ M.keys r
                values = UA.listArray (1, M.size r) $ M.elems r


    trainItem r
        | M.size r > 1 = do
            svm <- computeUsvm
            km <- gets ukf-- get kernel matrix vector
            let sol = solve svm dataset eps iterNum
                ps = simulate svm sol points
                outliers = filterIndex' (>1) $ zipWith ((abs .).(-)) ps (M.elems r)
                gradient = do
                    i <- [1..M.size r]
                    --j <- [1..i-1]
                    --(j, ej) <- diff ps values
                    j <- outliers
                    guard (i /= j)
                    let alp = alpha sol
                        --val = alp UA.! i * ej-- * values UA.! i 
                        val = alp UA.! i * alp UA.! j-- * values UA.! i * values UA.! j
                    return (pos (points UA.! i) (points UA.! j), step * val)
                --update :: ST s ()
                update v (p,g) = UMV.read v p >>= \val -> write v p $lm (val + g)
                updateAll v = mapM_ (update v) gradient
                nkm = UV.modify updateAll km
            modify (\k -> k{ukf = nkm})
            return $ Just sol
        | otherwise = return Nothing
            where
                dataset = DataSet points values
                points = A.listArray (1, M.size r) $ M.keys r
                values = UA.listArray (1, M.size r) $ M.elems r

    lm = limit (-1) 1
    diff :: [Double] -> UA.UArray Int Double -> [(Int, Double)]
    diff as bA = go (1::Int) as bA where
        go ind [] _ = []
        go ind (a:as) bA = (ind, bA UA.! ind - a) : go (ind+1) as bA
{-
    trainItem r
        | M.size r > 1 = do
            svm <- computeIsvm
            return $ Just $ solve svm dataset eps iterNum
        | otherwise = return Nothing
            where
                dataset = DataSet points values
                points = A.listArray (1, M.size r) $ M.keys r
                values = UA.listArray (1, M.size r) $ M.elems r
-}
    pos i j | i >= j = let val = (i-1) * i `quot` 2 + j - 1 in val
                            --if (val < 586986) then val else traceShow (i,j) val
            | otherwise = pos j i
    initState = Kernel ukfCache ikfCache where
        {-
        ukm <- gets ukf
        ikm <- gets ikf
        mapM (uncurry (write ukm)) ukfCache
        mapM (uncurry (write ikm)) ikfCache
        return () where -}
        ukfCache = UV.fromList [kf $ similarity urs ua ub | ua <- [1..nu], ub <- [1..ua]]
        ikfCache = UV.fromList [kf $ similarity irs ia ib | ia <- [1..ni], ib <- [1..ia]]
        kf a = a * (abs a)-- * (abs a)

    nu = V.length urs - 1
    ni = V.length irs - 1
    cost = 0.1
    params = []
    --step = 0.01


report :: [[(Double, Double, Double)]] -> [Double] -> FilePath -> IO ()
report pss values file = do
    hf <- openFile file WriteMode
    hPutStrLn hf $ "RMSE:\t" ++ (show $ last ers)
    print ers
    go hf (last pss) values
    hClose hf
    where
        ers = map (\ps -> rmse (map (\(a,b,c) -> c) ps) values) pss
        --conbine (a, b) = (a + b) / 2
        go _ [] [] = return ()
        go hf ((up, ip, p) : ps) (v:vs) = do
            hPutStrLn hf $ show (up, ip, p, v)
            go hf ps vs

rmse :: [Double] -> [Double] -> Double
rmse as bs = assert (len == length bs) (go as bs 0) where
    len = length as
    go :: [Double] -> [Double] -> Double -> Double
    go [] [] acc = sqrt (acc / fromIntegral len)
    go (a:as') (b:bs') acc = go as' bs' (acc + (a - b)^2)

main = do
    (ftr:fts:eps:itr:step:gen:_) <- getArgs
    {-
    train <- tuplesFromFile "data/ua.base"
    test <- tuplesFromFile "data/ua.test"
    -}
    train <- tuplesFromFile ftr
    test <- tuplesFromFile fts
    (urs, irs) <- Main.mlconvert train
    let res = run urs irs (read eps) (read itr) (read step) (read gen) test
        values = map (\(u,i,r) -> r) test
    report res values (fts ++ ".res")
--    print res
