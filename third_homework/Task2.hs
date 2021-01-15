module Task2 where
    import Data.Word    
    import Prelude
    import Data.List

    data Rgb = Rgb { red   :: Word8
                   , green :: Word8
                   , blue  :: Word8 } deriving (Show,Read)
    instance Eq Rgb where
        Rgb r1 g1 b1 == Rgb r2 g2 b2 = r1 == r2 && g1 == g2 && b1 == b2

    data Image = Image { width   :: Int
                       , height  :: Int
                       , content :: [[Rgb]] } deriving (Show,Read)
    instance Eq Image where
        Image w1 h1 m1 == Image w2 h2 m2 = w1 == w2 && h1 == h2 && m1 == m2

    ------------------Grayscale  functions------------------------------
    --------------------------------------------------------------------
    -- we take row n from Image content
    getrow :: Int -> Image -> [Rgb]
    getrow n img | length (content img) <= n = [] | otherwise = (content img) !! n

    -- from word8 to integer
    word8ToInteger :: Word8 -> Integer
    word8ToInteger = fromIntegral

    -- this is how we make from integer to float
    -- exampleFloat = fromInteger exampleInt :: Float


    -- from float to integer
    toInt :: Float -> Integer
    toInt x = round x

    -- from integer to word8
    integerToWord8 :: Integer -> Word8
    integerToWord8 = fromIntegral

    -- from Word8 to Float function
    word8ToFloat :: Word8 -> Float
    word8ToFloat x = fromInteger (word8ToInteger x) :: Float

    -- Float to Word8
    floatToWord8 :: Float -> Word8
    floatToWord8 x = integerToWord8 (toInt x)

    -- Takes untoucked Word8 and converts it to Grey Word8 (0.30*R + 0.59*G + 0.11*B)
    word8ToGreyWord8 :: Rgb -> Word8
    word8ToGreyWord8 x = floatToWord8 (((word8ToFloat (red x)) * 0.30) + ((word8ToFloat (green x)) * 0.59) + ((word8ToFloat (blue x)) * 0.11))


    -- Converts Rgb to grey Rgb
    convertRgbToGrayscale :: Rgb -> Rgb
    convertRgbToGrayscale img = Rgb (word8ToGreyWord8 img) (word8ToGreyWord8 img) (word8ToGreyWord8 img)


    grayscale :: Image -> Image
    grayscale img = Image (width img) (height img) (map (\row -> map (\x -> convertRgbToGrayscale x) row) (content img))


    ----------------------------------edgeDetect Functions ----------------------------------------
    -----------------------------------------------------------------------------------------------------

    -- Makes 3x3 matrix from element on pos X and Y in matrix
    -- If element is not in matrix we take the element +1 column element
    makeMatrix :: Int -> Int -> [[Rgb]]-> [[Word8]]
    makeMatrix row coll matrix = [[x1, y1, z1],
                                  [x2, y2, z2],
                                  [x3, y3, z3]] where
                                x1 | (row-1) >= 0 && (coll-1) >= 0 = (red (matrix!!(row-1)!!(coll-1))) | 
                                     (row + 1) < (length matrix) && (coll - 1) >= 0 = (red (matrix!!(row+1)!!(coll-1))) | 
                                     (row - 1) >= 0 && (coll + 1) < (length (matrix!!0)) = (red (matrix!!(row-1)!!(coll+1))) | 
                                     (row + 1) < (length matrix) && (coll + 1) < (length (matrix!!0)) =(red (matrix!!(row+1)!!(coll+1) )) | 
                                     otherwise = -1
                                y1 | (row - 1) >= 0 = (red (matrix!!(row-1)!!(coll))) |
                                     (row+1) < (length matrix) = (red (matrix!!(row+1)!!(coll))) |
                                     (coll-1) >= 0 = (red (matrix!!row!!(coll-1))) |
                                     (coll + 1) < (length (matrix!!0)) = (red (matrix!!row!!(coll+1))) |
                                     otherwise = -1
                                z1 | (row - 1) >= 0 && (coll + 1) < (length (matrix!!0)) = (red (matrix!!(row-1)!!(coll+1))) |
                                     (row - 1) >= 0 && (coll - 1) >= 0 = (red (matrix!!(row-1)!!(coll-1))) |
                                     (row + 1) < (length matrix) && (coll - 1) >=0 = (red (matrix!!(row+1)!!(coll-1))) |
                                     (row + 1) < (length matrix) && (coll + 1) < (length (matrix!!0)) = (red (matrix!!(row+1)!!(coll+1))) |
                                     otherwise = -1
                                x2 | (coll - 1) >= 0 = (red (matrix!!(row)!!(coll-1))) |
                                     (coll + 1) < (length (matrix!!0)) = (red (matrix!!row!!(coll+1))) |
                                     otherwise = -1
                                y2 = (red (matrix!!row!!coll))
                                z2 | (coll + 1) < (length (matrix!!0)) = (red (matrix!!(row)!!(coll+1))) | 
                                     (coll - 1) >= 0 = (red (matrix!!row!!(coll - 1))) |
                                     otherwise = x2
                                x3 | (row + 1) < (length matrix) && (coll - 1) >= 0 = (red (matrix!!(row+1)!!(coll-1))) |
                                     (row - 1) >= 0 && (coll - 1) >= 0 = (red (matrix!!(row-1)!!(coll-1))) | 
                                     otherwise = z1
                                y3 | (row + 1) < (length matrix) = (red (matrix!!(row+1)!!(coll))) |
                                     (row - 1) >= 0 = (red (matrix!!(row-1)!!(coll))) |
                                     otherwise = y1
                                z3 | (row + 1) < (length matrix) && (coll + 1) < (length (matrix!!0)) = (red (matrix!!(row+1)!!(coll+1))) |
                                     (row - 1) >= 0 && (coll + 1) < (length (matrix!!0)) = (red (matrix!!(row-1)!!(coll+1))) |
                                     otherwise = x1



    --First matrix to multiply 
    firstMatrx = [[1,0,-1],[2,0,-2], [1,0,-1]]
    --Second matrix to multiply 
    secondMatrx = [[1,2,1],[0,0,0],[-1,-2,-1]]

    -- Word8 to Int Converter
    word8ToInt :: Word8 -> Int
    word8ToInt = fromIntegral

    -- Int to Word8 Converter
    intToWord8 :: Int -> Word8
    intToWord8 = fromIntegral


    -- Scalar multiply the matrix m1[0][0] * m2[2][2] +  m1[0][1] * m2[2][1] .....
    multMatrix :: [[Word8]] -> [[Int]] -> Int
    multMatrix m1 m2 = ((word8ToInt (m1!!0!!0)) * m2!!2!!2) + 
                       ((word8ToInt (m1!!0!!1)) * m2!!2!!1) + 
                       ((word8ToInt (m1!!0!!2)) * m2!!2!!0) +
                       ((word8ToInt (m1!!1!!0)) * m2!!1!!2) +
                       ((word8ToInt (m1!!1!!1)) * m2!!1!!1) +
                       ((word8ToInt (m1!!1!!2)) * m2!!1!!0) +
                       ((word8ToInt (m1!!2!!0)) * m2!!0!!2) +
                       ((word8ToInt (m1!!2!!1)) * m2!!0!!1) +
                       ((word8ToInt (m1!!2!!2)) * m2!!0!!0)      


    -- givesRgb if <0 gives 0 elif >255 gives 255 else gives sqrt(firstMult^2 + secondMult^2)
    giveRGBValue :: Int -> Int -> [[Rgb]] -> Int
    giveRGBValue row coll matrix | x > 255 = 255 | x < 0 = 0 |otherwise = x where
        x = round (sqrt (fromIntegral (((multMatrix (makeMatrix row coll matrix) firstMatrx)^2 + (multMatrix (makeMatrix row coll matrix) secondMatrx)^2)) :: Float))

    -- calculates new pixel values
    calculateForPixel :: Int -> Int -> [[Rgb]] -> Rgb
    calculateForPixel row col matrix = Rgb x x x where
        x = (intToWord8 (giveRGBValue row col matrix))

    -- calculates for a row
    workWithRow :: Int -> Int -> Int -> [[Rgb]] -> [Rgb]
    workWithRow i numRow lenrow matrix | i == lenrow = [] | otherwise = [calculateForPixel numRow i matrix] ++ (workWithRow (i + 1) numRow lenrow matrix)

    -- gives rows to workWithRow
    walkTroughMatrix :: Int -> [[Rgb]] -> [[Rgb]]
    walkTroughMatrix i matrix | i == (length matrix) = [] | otherwise = (workWithRow 0 i (length (matrix!!0)) matrix) : (walkTroughMatrix (i + 1) matrix)




    edgeDetect :: Image -> Image
    edgeDetect img = Image (width img) (height img) (walkTroughMatrix 0 (content img))



    --------------------------------- floodFill Functions -----------------------------------------------------------
    -----------------------------------------------------------------------------------------------------------------

    -- Replaces Nth element in a row
    replaceNth :: Int -> Rgb -> [Rgb] -> [Rgb]
    replaceNth _ _ [] = []
    replaceNth n newVal (x:xs) | n == 0 = newVal:xs | otherwise = x:(replaceNth (n-1) newVal xs)

    -- replaces Nth element in a matrix (gives the X row to replaceNth)
    replaceXY :: Int -> Int -> Int -> Rgb -> [[Rgb]] -> [[Rgb]]
    replaceXY x y i color matrix | i == (length matrix) = [] | i /= x = (matrix!!i) : (replaceXY x y (i+1) color matrix) 
                                 | otherwise = (replaceNth y color (matrix!!i)) : (replaceXY x y (i+1) color matrix)


    -- checks if element is with color equal to startColor and if so it checks +1 -1 cols and +1 -1 rows to seee if it can be colored 
    checkIfNearPainted :: Int -> Int -> Rgb -> Rgb -> [[Rgb]] -> Bool
    checkIfNearPainted x y color startColor matrix | (matrix!!x!!y) /= startColor = False
                                                   | n == True = True
                                                   | s == True = True
                                                   | w == True = True
                                                   | e == True = True
                                                   | otherwise = False where
                                                    n | (x - 1) >=0 && (matrix!!(x-1)!!y) == color = True | otherwise = False
                                                    s | (x + 1) < (length matrix) && (matrix!!(x+1)!!y) == color = True | otherwise = False
                                                    w | (y - 1) >=0 && (matrix!!x!!(y-1)) == color = True | otherwise = False
                                                    e | (y + 1) < (length (matrix!!0)) && (matrix!!x!!(y+1)) == color = True | otherwise = False



    --if its in the end x and y it returns the matrix
    --if x y element can be colored it calls the function again  from 0 0 and with new matrix where the x y position is colored now
    --otherwise it calls the function for the next element 
    floodFillAlgHelper :: Int -> Int  -> Rgb -> Rgb -> [[Rgb]] -> [[Rgb]]
    floodFillAlgHelper x y  color startColor matrix | x == ((length matrix) - 1) && y == ((length (matrix!!0)) - 1) = matrix
                                                    | (checkIfNearPainted x y color startColor matrix) == True = (floodFillAlgHelper 0 0 color startColor (replaceXY x y 0 color matrix)) 
                                                    | otherwise = (floodFillAlgHelper row coll color startColor matrix) where
                                                      row | y == ((length (matrix!!0)) - 1) && x /= ((length matrix) - 1) = (x + 1) | otherwise = x
                                                      coll | y == ((length (matrix!!0)) - 1) = 0 | otherwise = (y + 1)

    -- floodFillAlgHelper returns a matrix
    -- if returned matrix is diff from the original matrix this means that some element was colored and we need to walk again to check
    -- if the new matrix is equal to the old one this means that everything is colored and we are done
    floodFillAlg :: Rgb -> Rgb -> [[Rgb]] -> [[Rgb]]
    floodFillAlg color startColor matrix | newMatrix == matrix = matrix
                                         | otherwise = (floodFillAlg color startColor newMatrix) where 
                                            newMatrix = (floodFillAlgHelper 0 0  color startColor matrix) 


    -- it calls floodFillAlg but first we take the starting element RGB and we color the starting element 
    floodFill :: Rgb -> Int -> Int -> Image -> Image
    floodFill color x y img  = Image (width img) (height img) (floodFillAlg color ((content img)!!x!!y) (replaceXY x y 0 color (content img)))



    ------------------ SaveImage Functions ---------------------------
    ------------------------------------------------------------------

    -- creates the needed string from the Image
    make_ppm :: [[Rgb]] -> String
    make_ppm [[]] = ""
    make_ppm matrix = "P3\n" ++ (show (length (matrix!!0))) ++ " " ++ (show (length matrix)) ++ "\n255\n" ++
        foldr (\row res -> (foldr (\el line -> (show (red el)) ++ " " ++ (show (green el)) ++ " " ++ (show (blue el)) ++ "\n" ++ line ) "" row) ++ res ++ "\n"  ) "" matrix


    saveImage :: FilePath -> Image -> IO ()
    saveImage path img = writeFile path $ make_ppm (content img)

