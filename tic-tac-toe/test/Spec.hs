import Test.Hspec
import Test.QuickCheck

import Board
import Tile

emptyBoard = [[Empty, Empty, Empty], 
              [Empty, Empty, Empty], 
              [Empty, Empty, Empty]]

fullBoard = [[X, O, X], 
             [O, X, O], 
             [O, X, O]]

diagGame = [[X, O, X], 
            [O, X, O], 
            [O, X, X]]

diagGame2 = [[O, X, X],
             [X, O, X],
             [O, X, O]]

invDiagGame = [[X, O, X], 
               [O, X, O], 
               [X, X, O]]

col1Game = [[X, O, X], 
            [X, X, O], 
            [X, X, O]]

col2Game = [[X, X, O], 
            [O, X, O], 
            [X, X, O]]

col3Game = [[X, O, X], 
            [X, X, O], 
            [X, X, O]]

row1Game = [[X, X, X],
            [O, O, X],
            [O, X, O]]

row2Game = [[O, O, X],
            [X, X, X],
            [O, X, O]]

row3Game = [[O, X, O],
            [X, O, O],
            [X, X, X]]

main :: IO ()
main = hspec $ do
    describe "#setBoard" $ do
        it "when init board" $ initBoard `shouldBe` emptyBoard
        describe "#set value into board" $ do
            it "when value is not empty" $ setBoard (Pos { row = 0, col = 0 }, fromEnum X) fullBoard `shouldBe` fullBoard
            it "when value is empty" $ setBoard (Pos { row = 0, col = 0 }, fromEnum X) emptyBoard `shouldBe` [[X, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Empty]]
    describe "#checking results" $ do
        it "row 1" $ checkWinners row1Game `shouldBe` True
        it "row 2" $ checkWinners row2Game `shouldBe` True
        it "row 3" $ checkWinners row3Game `shouldBe` True
        it "col 1" $ checkWinners col1Game `shouldBe` True
        it "col 2" $ checkWinners col2Game `shouldBe` True
        it "col 3" $ checkWinners col3Game `shouldBe` True
        it "diag" $ checkWinners diagGame `shouldBe` True
        it "diag2" $ checkWinners diagGame2 `shouldBe` True
        it "inv Diag" $ checkWinners invDiagGame `shouldBe` True
        it "init game" $ checkWinners emptyBoard `shouldBe` False
        it "no winners" $ checkWinners fullBoard `shouldBe` False
    describe "#simulating a game" $ do
        it "O wins" $ 
            shouldBe True $ 
            checkWinners $
            setBoard (Pos { row = 0, col = 0 }, fromEnum X) $
            setBoard (Pos { row = 0, col = 1 }, fromEnum O) $
            setBoard (Pos { row = 0, col = 2 }, fromEnum X) $
            setBoard (Pos { row = 1, col = 1 }, fromEnum O) $
            setBoard (Pos { row = 1, col = 0 }, fromEnum X) $
            setBoard (Pos { row = 2, col = 1 }, fromEnum O) emptyBoard
        it "X wins" $
            shouldBe True $
            checkWinners $
            setBoard (Pos { row = 0, col = 0 }, fromEnum O) $
            setBoard (Pos { row = 0, col = 1 }, fromEnum X) $
            setBoard (Pos { row = 0, col = 2 }, fromEnum O) $
            setBoard (Pos { row = 1, col = 1 }, fromEnum X) $
            setBoard (Pos { row = 1, col = 0 }, fromEnum O) $
            setBoard (Pos { row = 2, col = 1 }, fromEnum X) emptyBoard
    describe "#check Game Over" $ do
        it "Over" $ checkGameOver fullBoard `shouldBe` True
        it "Over" $ checkGameOver diagGame `shouldBe` True
        it "Not Over" $ checkGameOver emptyBoard `shouldBe` False
            
