module Tests (
    main
) where

import Data.Char
import Control.Monad
import System.IO
import System.Directory
import Test.HUnit     
import Test.QuickCheck
import Main hiding (main)

todoCases = TestLabel "Todo test cases" ( TestList [
               testTodoIsValid1, testTodoIsValid2, testAddTodo, testDeleteTodo, testGetTodoPriorityOne
            ] )

main = runTestTT $ TestList [todoCases]

-- 
-- Validating items
-- 

testTodoIsValid1 = TestCase $ assertEqual "Todo should be invalid" False 
    (todoIsValid ["invalid", "no", "uppercase"])
testTodoIsValid2 = TestCase $ assertEqual "Todo should be valid" True
    (todoIsValid ["uppercase", "Valid"])

-- 
-- Adding items
-- 

testAddTodo = TestCase (do 
    deleteAll "testDB" "todo" 
    add "testDB" "todo" (Just ["first", "Here"])
    results <- get "testDB" ["todo"]
    case results of 
        Nothing -> assertEqual "Didn't get any results after adding todo" True False
        Just strings -> assertEqual "DB should hold one todo" 1 (length strings))

-- 
-- Deleting items
-- 

testDeleteTodo = TestCase (do
    deleteAll "testDB" "todo"
    add "testDB" "todo" (Just ["tag1", "First", "here"])
    add "testDB" "todo" (Just ["second", "Here"])
    add "testDB" "todo" (Just ["tag2", "Third"])
    deleteItem "testDB" ["todo", "2"]
    results <- get "testDB" ["todo"]
    case results of 
        Nothing -> assertEqual "Didn't get any todos from the DB" True False
        Just strings -> assertEqual "The second todo should have been deleted" (2, "1 - First here", "2 - Third") tup
            where tup = (length strings, head strings, strings !! 1))

--
-- Todo prorities
--

testGetTodoPriorityOne = TestCase (do
    deleteAll "testDB" "todo"
    add "testDB" "todo" (Just ["tag1", "p1", "First", "here"])
    add "testDB" "todo" (Just ["tag1", "p2", "Second", "here"])
    add "testDB" "todo" (Just ["tag1", "Third", "here"])
    results <- get "testDB" ["todo", "p1"]
    case results of 
        Nothing -> assertEqual "Didn't get any results after adding todo with priority 1" True False
        Just strings -> assertEqual "There should be only one todo with priority 1" 1 (length strings))

-- 
-- Tags
--

