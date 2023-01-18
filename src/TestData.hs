module TestData
    ( testQuery
    , testQueryFail
    , testElem
    , testRelation3
    , testRelation2a
    , testRelation2b
    , testList2a
    , testList2b
    , testList3
    ) where

import Logic

testQuery :: RelationQuery Int
testQuery = RelQuery [Fixed 1, Fixed 2, Variable]
testQueryFail :: RelationQuery Int
testQueryFail = RelQuery [Fixed 1, Fixed 3, Variable]
testElem :: RelationItem Int
testElem = RelItem [1, 2, 3]
testRelation3 :: Relation Int
testRelation3 = Rel [RelItem [1, 2, 3], RelItem [4, 5, 6], RelItem [7, 8, 9]]
testRelation2a :: Relation Int
testRelation2a = Rel [RelItem [1, 3], RelItem [2, 3], RelItem [3, 4]]
testRelation2b :: Relation Int
testRelation2b = Rel [RelItem [4, 5], RelItem [3, 6]]
testList2a :: [[Int]]
testList2a = [[1, 3], [2, 3], [3, 4]]
testList2b :: [[Int]]
testList2b = [[4, 5], [4, 6], [3, 7]]
testList3 :: [[Int]]
testList3 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]