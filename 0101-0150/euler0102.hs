-- https://projecteuler.net/problem=102
--
-- Three distinct points are plotted at random on a Cartesian plane, 
-- for which -1000 ≤ x, y ≤ 1000, such that a triangle is formed.
-- Consider the following two triangles:
--    A(-340,495), B(-153,-910), C(835,-947)
--    X(-175,41), Y(-421,-714), Z(574,-645)
-- It can be verified that triangle ABC contains the origin, whereas triangle XYZ does not.
-- Using triangles.txt (right click and 'Save Link/Target As...'), a 27K text file containing the co-ordinates of one 
-- thousand "random" triangles, find the number of triangles for which the interior contains the origin.
-- NOTE: The first two examples in the file represent the triangles in the example given above.


data Point = Point {
    x :: Integer,
    y :: Integer
} deriving (Show, Eq)


data Triangle = Triangle {
    a :: Point,
    b :: Point,
    c :: Point
} deriving (Show, Eq)


one :: Float
one = 1


inTriangle :: Point -> Triangle -> Bool
inTriangle p t = d > 0 && e > 0 && f > 0
    where
        p1 = a t
        p2 = b t
        p3 = c t
        d = (fromIntegral ((y p2 - y p3) * (x p - x p3) + (x p3 - x p2) * (y p - y p3)) ) / (fromIntegral ((y p2 - y p3) * (x p1 - x p3) + (x p3 - x p2) * (y p1 - y p3)))
        e = (fromIntegral ((y p3 - y p1) * (x p - x p3) + (x p1 - x p3) * (y p - y p3))) / (fromIntegral ((y p2 - y p3) * (x p1 - x p3) + (x p3 - x p2) * (y p1 - y p3)))
        f = one - d - e


toTriangle :: [Integer] -> Triangle
toTriangle i = Triangle {a = Point{x = x1, y = y1}, b = Point{x = x2, y = y2}, c = Point{x = x3, y = y3}}
    where
        x1 = head i
        y1 = head (tail i)
        ni1 = tail (tail i)
        x2 = head ni1
        y2 = head (tail ni1)
        ni2 = tail (tail ni1)
        x3 = head ni2
        y3 = head (tail ni2)

origin :: Point
origin = Point{x = 0, y = 0}


trianglesWithPoint :: Point -> [Triangle] -> [Triangle]
trianglesWithPoint p ts = filter (\t -> inTriangle p t) ts


numberOfTrianglesWithPoint :: Point -> [Triangle] -> Int
numberOfTrianglesWithPoint p ts = length (trianglesWithPoint p ts)


splitOn :: Char -> String -> String -> [String]
splitOn _ [] acc = [acc]
splitOn c (s:str) acc = if c == s then acc : splitOn c str [] else splitOn c str (acc ++ [s])


toIntegers :: String -> [Integer]
toIntegers text = map (\s -> read s :: Integer) (splitOn ',' text "")


main :: IO ()
main = do
  f <- readFile "../files/p102_triangles.txt"
  let ints = [toIntegers s | s <- lines f]
  let triangles = map toTriangle ints

  print ("Expected: " ++ show 228 ++ ", actual: " ++ show (numberOfTrianglesWithPoint origin triangles))
