import SVM
import qualified Data.Vector as V
import Data.Array
import qualified Data.Array.Unboxed as UA
import qualified Data.Vector.Unboxed as UV


svm = LSSVM (KernelFunction mykf) 0.5 [1, 5, 1, 1]

mykf :: [Double] -> [Double] -> [Double] -> Double
mykf = polyKernelFunction

sol = solve svm dataset 0.001 100 where
    dataset = DataSet points values
    {-
    points :: Vector [Double]
    points = fromList [[-1, -1], [-1,1], [1,-1], [1,1]]
    values = UV.fromList [-1::Double, 1, 1, -1]
    -}
    points = listArray (1,4) [[-1, -1], [-1,1], [1,-1], [1,1]]
    values = UA.listArray (1,4) [-1::Double, 1, 1, -1]


sim = simulate svm sol
