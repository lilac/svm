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
import Data.Vector.Mutable as MV hiding (length)
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

convert :: [RTuple] -> IO (V.Vector UserRating, V.Vector ItemRating)
convert ts = construct where
    (nu, ni) = foldl' maxTuple (0, 0) ts
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

similarity :: Vec (Map Int Double) -> Int -> Int -> Double
similarity v i j = go (S.elems $ S.intersection (keysSet mi)  (keysSet mj)) 0 0 0 where
    mi = v V.! i
    mj = v V.! j
    avgs = V.map avg v -- TODO: optimize this
    go [] ac1 ac2 ac3
        | ac2 == 0 || ac3 == 0 = 0
        | otherwise = ac1 / (sqrt ac2 * sqrt ac3)
    go (ck:cks) ac1 ac2 ac3 = go cks ac1' ac2' ac3' where
        ac1' = ac1 + (mi ! ck - avgs V.! i) * (mj ! ck - avgs V.! j)
        ac2' = ac2 + (mi ! ck - avgs V.! i) ^ 2
        ac3' = ac3 + (mj ! ck - avgs V.! j) ^ 2

newLSSVM :: Vec UserRating -> Vec ItemRating -> LSSVM [Int]
newLSSVM urs irs = LSSVM (KernelFunction kf) cost params where
    kf :: [Double] -> [Int] -> [Int] -> Double
    kf (uw:iw:_) (ua:ia:_) (ub:ib:_) = let val = (usim UV.! (pos ua ub) * uw + isim UV.! (pos ia ib) * iw) in val * abs val ** 1.5
    pos i j | i >= j = let val = (i-1) * i `quot` 2 + j - 1 in val
                            --if (val < 586986) then val else traceShow (i,j) val
            | otherwise = pos j i
    usim = UV.fromList [similarity urs ua ub | ua <- [1..nu], ub <- [1..ua]]
    isim = UV.fromList [similarity irs ia ib | ia <- [1..ni], ib <- [1..ia]]
    nu = V.length urs - 1
    ni = V.length irs - 1

    cost = 0.5
    params = let
        dnu = fromIntegral nu :: Double
        dni = fromIntegral ni :: Double
        s = dnu + dni in [dnu / s, dni / s]

runLSSVM :: LSSVM [Int] -> Double -> Int -> [RTuple] -> SVMSolution [Int]
runLSSVM svm eps iterNum !rts = solve svm dataset eps iterNum where
    dataset = DataSet points values
    --points :: A.Array Int [Int]
    points = A.listArray (1::Int, len) features
    --points = V.fromList features
    features = map (\(u,i,r) -> [u::Int,i]) rts :: [[Int]]
    --values :: UA.UArray Int Double
    values = UA.listArray (1::Int, len) $ map (\(u,i,r) -> r) rts
    --values = UV.fromList $ map (\(u,i,r) -> r) rts
    len = length rts :: Int


simLSSVM :: LSSVM [Int] -> SVMSolution [Int] -> [RTuple] -> [Double]
simLSSVM !svm sol rts = simulate svm sol points {-`rmse` values-} where

    points = A.listArray (1::Int, length rts) $ map (\(u,i,r) -> [u,i]) rts
    --points = V.fromList $ map (\(u,i,r) -> [u,i]) rts
    values = map (\(u,i,r) -> r) rts

report :: SVMSolution [Int] -> [Double] -> [Double] -> FilePath -> IO ()
report sol predictions values file = do
    hf <- openFile file WriteMode
    hPutStrLn hf $ "Num of SV:\t" ++ (show . snd . UA.bounds . sv) sol
    hPutStrLn hf $ "RMSE:\t" ++ er
    putStrLn er
    go hf predictions values
    hClose hf
    where
        er = show (rmse predictions values)
        go _ [] [] = return ()
        go hf (p:ps) (v:vs) = do
            hPutStrLn hf $ show (p,v, p - v)
            go hf ps vs

rmse :: [Double] -> [Double] -> Double
rmse as bs = assert (len == length bs) (go as bs 0) where
    len = length as
    go :: [Double] -> [Double] -> Double -> Double
    go [] [] acc = sqrt (acc / fromIntegral len)
    go (a:as') (b:bs') acc = go as' bs' (acc + (a - b)^2)

main = do
    (ftr:fts:_) <- getArgs
    {-
    train <- tuplesFromFile "data/ua.base"
    test <- tuplesFromFile "data/ua.test"
    -}
    train <- tuplesFromFile ftr
    test <- tuplesFromFile fts
    (urs, irs) <- Main.convert train
    let svm = newLSSVM urs irs
        sol = runLSSVM svm 0.08 10 train
        res = simLSSVM svm sol test
        values = map (\(u,i,r) -> r) test
    report sol res values (fts ++ ".res")
--    print res
