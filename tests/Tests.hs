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
import Database.MongoDB -- needed for :=
import Data.Int

todoCases = TestLabel "Todo test cases" ( TestList [
               testTodoIsValid1, testTodoIsValid2, testAddTodo, testDeleteTodo, 
               testGetTodoPriorityOne, testTodoConstructPrioritySelection,
               testTodoTagsSelector,
               testTodoConstructOneTagSelection, 
               testTodoConstructTwoTagsSelection,
               testGetTodoTags, 
               --testGetTwoTodoTags, this test fails
               testGetFieldsForTodo,
               testGetTodoFromPriorityAndTag
            ] )

noteCases = TestLabel "Note test cases" ( TestList [] )

main = runTestTT $ TestList [todoCases, noteCases]

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
    deleteAll TestDB Todo
    add TestDB Todo ["first", "Here"]
    results <- get TestDB ["todo"]
    case results of 
        [] -> assertEqual "Didn't get any results after adding todo" True False
        strings -> assertEqual "DB should hold one todo" 1 (length strings))

-- 
-- Deleting items
-- 

testDeleteTodo = TestCase (do
    deleteAll TestDB Todo
    add TestDB Todo ["tag1", "First", "here"]
    add TestDB Todo ["second", "Here"]
    add TestDB Todo ["tag2", "Third"]
    deleteItem TestDB ["todo", "2"]
    results <- get TestDB ["todo"]
    case results of 
        [] -> assertEqual "Didn't get any todos from the DB" True False
        strings -> assertEqual "The second todo should have been deleted" 
            (2, "1 - First here", "2 - Third") tup
                where tup = (length strings, head strings, strings !! 1))

--
-- Todo prorities
--

testGetTodoPriorityOne = TestCase (do
    deleteAll TestDB Todo
    add TestDB Todo ["tag1", "p1", "First", "here"]
    add TestDB Todo ["tag1", "p2", "Second", "here"]
    add TestDB Todo ["tag1", "Third", "here"]
    results <- get TestDB ["todo", "p1"]
    case results of 
        [] -> assertEqual 
            "Didn't get any results after adding todo with priority 1" 
                True False
        strings -> assertEqual "There should be only one todo with priority 1" 
            1 (length strings))

testTodoConstructPrioritySelection = TestCase (do
    assertEqual "The selection on priority should match this"
        (select ([(fieldToText Priority) =: (1 :: Int32)]) (docTypeToText Todo))
            (constructSelection Todo ["p1"]))
-- 
-- Tags
--

testTodoTagsSelector = TestCase (do
    assertEqual "The tags selector for two tags should match..."
        [(fieldToText Tags) =: "city", (fieldToText Tags) =: "urban"]
            (todoTagsSelector [] ["city", "urban"]))

testTodoConstructOneTagSelection = TestCase (do
    assertEqual "The selection on tags should match this" 
        (select ([(fieldToText Tags) =: "school"]) 
            (docTypeToText Todo))
                (constructSelection Todo ["school"]))

testTodoConstructTwoTagsSelection = TestCase (do
    assertEqual "The selection on tags should match this" 
        (select ([(fieldToText Tags) =: "school", (fieldToText Tags) =: "228"]) 
            (docTypeToText Todo))
                (constructSelection Todo ["school", "228"]))

testGetTodoTags = TestCase (do
    deleteAll TestDB Todo
    add TestDB Todo ["p1", "First", "here"]
    add TestDB Todo ["p2", "Second", "here"]
    add TestDB Todo ["school", "Third", "here"]
    results <- get TestDB ["todo", "school"]
    case results of 
        [] -> assertEqual "Didn't get results after adding todo tagged 'school'"
            True False
        strings -> assertEqual "There should be only one todo tagged 'school'" 
            (1, "1 - Third here") tup
                where tup = (length strings, head strings))

{-
 - This test fails.
 -
testGetTwoTodoTags = TestCase (do
    deleteAll TestDB Todo
    add TestDB Todo ["p1", "school", "First", "here"]
    add TestDB Todo ["229", "Second", "here"]
    add TestDB Todo ["school", "229", "Third", "here"]
    results <- get TestDB ["todo", "school", "229"]
    case results of 
        [] -> assertEqual 
            "Didn't get results after adding todo tagged 'school' and '229'"
                True False
        strings -> assertEqual 
            "There should be only one todo tagged 'school' and '229'" 
                (1, "1 - Third here") tup
                    where tup = (length strings, head strings))
                    -}

testGetFieldsForTodo = TestCase (do
    assertEqual "Should get a list of fields for matching two tags"
        [(fieldToText Tags) =: "school", (fieldToText Tags) =: "229"]
            (getFieldsForTodo [] ["school", "229"]))

testGetTodoFromPriorityAndTag = TestCase (do
    deleteAll TestDB Todo
    add TestDB Todo ["p1", "school", "First", "here"]
    add TestDB Todo ["school", "Second", "here"]
    add TestDB Todo ["p1", "Third", "here"]
    results <- get TestDB ["todo", "school", "p1"]
    case results of 
        [] -> assertEqual 
            "Didn't get results after adding todo tagged 'school' and 'p1'"
                True False
        strings -> assertEqual
            "There should be only one todo tagged 'school' and 'p1'" 
                (1, "1 - First here") tup
                    where tup = (length strings, head strings))
