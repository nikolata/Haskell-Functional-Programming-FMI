import Task1
import Test.HUnit

simpleExample :: Tree Int
simpleExample = Node {value=2, 
                left=Node{value=1, left=EmptyTree, right=EmptyTree},
                right=Node{value=3, left=EmptyTree, right=EmptyTree}}


example :: Tree Int
example = Node {value=5, 
                left=Node{value=22, 
                          left=Node{value=2, 
                                    left=EmptyTree, 
                                    right=EmptyTree}, 
                          right=Node{value=6, 
                                     left=EmptyTree, 
                                     right=EmptyTree}
                         }, 
                right=Node{value=1,
                           left=EmptyTree, 
                           right=Node{value=3, 
                                      left=Node{value=111, 
                                                left=EmptyTree, 
                                                right=EmptyTree}, 
                                      right=EmptyTree}
                           }
                }
simpleTest = TestCase $ do
    assertEqual "Simple Inorder" [1,2,3] (values Inorder simpleExample)
    assertEqual "Simple Preorder" [2,1,3] (values Preorder simpleExample)
    assertEqual "Simple Postorder" [1,3,2] (values Postorder simpleExample)

exampleTest = TestCase $  do 
    assertEqual "Inorder Example = [2,22,6,5,1,111,3]" [2,22,6,5,1,111,3] (values Inorder example)
    assertEqual "Preorder Example = [5,22,2,6,1,3,111]" [5,22,2,6,1,3,111] (values Preorder example)
    assertEqual "Postorder Example = [2,6,22,111,3,1,5]" [2,6,22,111,3,1,5] (values Postorder example)

tests = TestList [simpleTest, exampleTest]

main = runTestTT tests