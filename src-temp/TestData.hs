module TestData
    ( testQuery
    , testQueryFail
    , testItem1a
    , testItem2a
    , testItem3a
    , testItem3b
    , testRelation3a
    , testRelation2a
    , testRelation2b
    , testList2a
    , testList2b
    , testList3a
    ) where

import RelationItem (RelationItem(RelItem))
import Relation (Relation)
import qualified Relation as R
import RelationQuery (QueryElement(Fixed, Variable), RelationQuery)
import qualified ChurchList as CL


testQuery :: RelationQuery Int
testQuery = RelItem [Fixed 1, Fixed 2, Variable]
testQueryFail :: RelationQuery Int
testQueryFail = RelItem [Fixed 1, Fixed 3, Variable]
testItem1a :: RelationItem Int
testItem1a = RelItem [1]
testItem2a :: RelationItem Int
testItem2a = RelItem [1, 2]
testItem3a :: RelationItem Int
testItem3a = RelItem [1, 2, 3]
testItem3b :: RelationItem Int
testItem3b = RelItem [4, 5, 6]
testRelation3a :: Relation Int
testRelation3a = CL.fromList [RelItem [1, 2, 3], RelItem [4, 5, 6], RelItem [7, 8, 9]]
testRelation2a :: Relation Int
testRelation2a = CL.fromList [RelItem [1, 3], RelItem [2, 3], RelItem [3, 4]]
testRelation2b :: Relation Int
testRelation2b = CL.fromList [RelItem [4, 5], RelItem [3, 6]]
testList2a :: [[Int]]
testList2a = [[1, 3], [2, 3], [3, 4]]
testList2b :: [[Int]]
testList2b = [[4, 5], [4, 6], [3, 7]]
testList3a :: [[Int]]
testList3a = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]