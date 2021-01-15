import Task2
import Test.HUnit


simpleExample = Image 2 1 [[Rgb 0 0 0, Rgb 255 255 255]]
example = Image 3 2 [[Rgb 255 0 0, Rgb 155 128 0,   Rgb 255 255 0],[Rgb 0 255 0, Rgb 255 255 255, Rgb 128 255 128]]

grayscaledExample = Image 3 2 [[Rgb  76 76 76,Rgb  122 122 122,Rgb  227 227 227],
                                                [Rgb  150 150 150,Rgb  255 255 255,Rgb  203 203 203]]


grayscaleTests = TestCase $ do
    assertEqual "Simple Grayscale" (Image 2 1 [[Rgb 0 0 0, Rgb 255 255 255]]) (grayscale simpleExample)
    assertEqual "Example Grayscale" (Image 3 2 [[Rgb {red = 76, green = 76, blue = 76},Rgb {red = 122, green = 122, blue = 122},Rgb {red = 227, green = 227, blue = 227}],
                                                [Rgb {red = 150, green = 150, blue = 150},Rgb {red = 255, green = 255, blue = 255},Rgb {red = 203, green = 203, blue = 203}]])
                                        (grayscale example)


edgeDetectTests = TestCase $ do
    assertEqual "Empty Image" (Image 0 0 [[]]) (edgeDetect (Image 0 0 [[]]))
    assertEqual "Simple Image" (Image 1 1 [[Rgb 0 0 0]]) (edgeDetect (Image 1 1 [[Rgb 1 1 1]]))
    assertEqual "Grayscaled example" (Image 3 2 [[Rgb 0 0 0, Rgb 255 255 255, Rgb 0 0 0],[Rgb 0 0 0, Rgb 255 255 255, Rgb 0 0 0]])
                                                (edgeDetect grayscaledExample)

floodFillTests = TestCase $ do
    assertEqual "Only one element to fill" (Image 3 3 [[Rgb 255 0 0, Rgb 155 128 0,   Rgb 255 255 0],
                                                       [Rgb 0 255 0, Rgb 5 5 5, Rgb 128 255 128],
                                                       [Rgb 0 255 0, Rgb 200 200 200, Rgb 128 255 128]] )
                                            (floodFill (Rgb 5 5 5) 1 1 (Image 3 3 [[Rgb 255 0 0, Rgb 155 128 0,   Rgb 255 255 0],
                                                                                   [Rgb 0 255 0, Rgb 255 255 255, Rgb 128 255 128],
                                                                                   [Rgb 0 255 0, Rgb 200 200 200, Rgb 128 255 128]]))

    assertEqual "Three elements to fill" (Image 3 3 [[Rgb 5 5 5, Rgb 5 5 5,   Rgb 255 255 0],
                                                     [Rgb 0 255 0, Rgb 5 5 5, Rgb 128 255 128],
                                                     [Rgb 0 255 0, Rgb 200 200 200, Rgb 128 255 128]])
                                            (floodFill (Rgb 5 5 5) 1 1 (Image 3 3 [[Rgb 255 255 255, Rgb 255 255 255, Rgb 255 255 0],
                                                                                   [Rgb 0 255 0, Rgb 255 255 255, Rgb 128 255 128],
                                                                                   [Rgb 0 255 0, Rgb 200 200 200, Rgb 128 255 128]]))

    assertEqual "Complex filling" (Image 3 3 [[Rgb 5 5 5, Rgb 5 5 5,   Rgb 255 255 0],
                                              [Rgb 0 255 0, Rgb 5 5 5, Rgb 5 5 5],
                                              [Rgb 255 255 255, Rgb 200 200 200, Rgb 128 255 128]])
                                            (floodFill (Rgb 5 5 5) 1 1 (Image 3 3 [[Rgb 255 255 255, Rgb 255 255 255, Rgb 255 255 0],
                                                                                   [Rgb 0 255 0, Rgb 255 255 255, Rgb 255 255 255],
                                                                                   [Rgb 255 255 255, Rgb 200 200 200, Rgb 128 255 128]]))


tests = TestList [grayscaleTests, edgeDetectTests, floodFillTests]

main = runTestTT tests