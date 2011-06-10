{-# LANGUAGE BangPatterns #-}
import qualified Text.Parsec.Token as T
import Text.Parsec.Combinator
{--
import Text.Parsec.Char
import Text.Parsec.String
--}
import Text.ParserCombinators.Parsec
import Text.Parsec.Language
import Data.Char
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Data.Vector hiding (forM_, foldl', (!), map, length, (++))
import qualified Data.Vector.Mutable as MV hiding (length)
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

type User = Int
type Item = Int
type Rating = Double

type RTuple = (User, Item, Rating)

type UserRating = M.Map Item Rating
type ItemRating = M.Map User Rating

type Vec = V.Vector
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

run :: Vec UserRating -> Vec ItemRating -> Double -> Int -> [RTuple] -> [(Double, Double, Double)]
run urs irs eps iterNum rts = ups where
    ups = map mypredict rts
    mypredict (u,i,r) = (up, ip, p) where
        up = case usvrs V.! u of
                Just sol -> predict isvm sol i
                Nothing -> gavg urs
        ip = case isvrs V.! i of
                Just sol -> predict usvm sol u
                Nothing -> gavg urs
        ua = uavgs V.! u
        ia = iavgs V.! i
        p | fit up && fit ip = if abs (up - ua) > abs (ip - ia) then ip else up
          | fit up = up
          | fit ip = ip
          | otherwise = (ia + ua) / 2
    usvrs = V.map (go isvm) urs
    isvrs = V.map (go usvm) irs
    usvm = LSSVM (KernelFunction ukf) cost params
    ukf _ ua ub = ukfCache UV.! (pos ua ub)

    isvm = LSSVM (KernelFunction ikf) cost params
    ikf _ ia ib = ikfCache UV.! (pos ia ib)
    
    uavgs = V.map avg urs
    iavgs = V.map avg irs
    go svm r
        | M.size r > 1 = Just $ solve svm dataset eps iterNum
        | otherwise = Nothing
            where
                dataset = DataSet points values
                points = A.listArray (1, M.size r) $ M.keys r
                values = UA.listArray (1, M.size r) $ M.elems r
    pos i j | i >= j = let val = (i-1) * i `quot` 2 + j - 1 in val
                            --if (val < 586986) then val else traceShow (i,j) val
            | otherwise = pos j i
    ukfCache = UV.fromList [kf $ similarity urs ua ub | ua <- [1..nu], ub <- [1..ua]]
    ikfCache = UV.fromList [kf $ similarity irs ia ib | ia <- [1..ni], ib <- [1..ia]]
    kf a = a * (abs a)-- * (abs a)
    nu = V.length urs - 1
    ni = V.length irs - 1
    cost = 0.1
    params = []


report :: [(Double, Double, Double)] -> [Double] -> FilePath -> IO ()
report predictions values file = do
    hf <- openFile file WriteMode
    hPutStrLn hf $ "RMSE:\t" ++ er
    putStrLn er
    go hf predictions values
    hClose hf
    where
        er = show (rmse (map (\(a,b,c) -> c) predictions) values)
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
    (ftr:fts:eps:itr:_) <- getArgs
    {-
    train <- tuplesFromFile "data/ua.base"
    test <- tuplesFromFile "data/ua.test"
    -}
    train <- tuplesFromFile ftr
    test <- tuplesFromFile fts
    (urs, irs) <- Main.mlconvert train
    let res = run urs irs (read eps) (read itr) test
        values = map (\(u,i,r) -> r) test
    report res values (fts ++ ".res")
--    print res
