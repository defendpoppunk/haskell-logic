module Relatives
    ( Person(Sarah, John, Arnold, Anne, Alex, Spot, Kevin, Artemis)
    , parentsList
    , parentsRelation
    , petsList
    , petsRelation
    , isParent
    , getChildren
    , getParents
    , getGrandchildren
    , getChildrensPets
    , getChildrenAndParents
    , getSiblings
    ) where

import Logic
import RelationItem (RelationItem)
import qualified RelationItem as RI
import Relation (Relation, relationToMatrix, matrixToRelation)
import RelationQuery (QueryElement(Fixed, Variable), RelationQuery)


data Person = Sarah | John | Arnold | Anne | Alex | Spot | Kevin | Artemis deriving (Show, Eq)


-- % parent(Parent, Child).
-- parent(sarah, john).
-- parent(arnold, john).
-- parent(john, anne).
-- parent(john, alex).
parentsList :: [[Person]]
parentsList = [ [Sarah, John]
              , [Arnold, John]
              , [John, Anne]
              , [John, Alex] 
              ]
parentsRelation :: Relation Person
parentsRelation = matrixToRelation parentsList

-- % parent(Owner, Pet).
-- parent(john, spot).
-- parent(alex, kevin).
-- parent(alex, artemis).
petsList :: [[Person]]
petsList = [ [John, Spot]
           , [Alex, Kevin]
           , [Alex, Artemis]
           ]
petsRelation :: Relation Person
petsRelation = matrixToRelation petsList


-- ?- parent(parent, child).
isParent :: Person -> Person -> Bool
isParent parent child = relationHasResults $ applyQuery parentsRelation query
    where query = RI.fromList [Fixed parent, Fixed child]

-- ?- parent(parent, X).
getChildren :: Person -> [[Person]]
getChildren parent = relationToMatrix $ applyQuery parentsRelation query
    where query = RI.fromList [Fixed parent, Variable]

-- ?- parent(X, child).
getParents :: Person -> [[Person]]
getParents child = relationToMatrix $ applyQuery parentsRelation query
    where query = RI.fromList [Variable, Fixed child]

-- grandparent(Grandparent, Grandchild) :- parent(Grandparent, X), parent(X, Grandchild).
-- ?- grandparent(grandparent, X).
getGrandchildren :: Person -> [[Person]]
getGrandchildren grandparent = relationToMatrix $ applyQuery grandparentsRelation query
    where grandparentsRelation = composeRelation parentsRelation parentsRelation 
          query = RI.fromList [Fixed grandparent, Variable]

-- childsPet(Parent, Pet) :- parent(Parent, X), pet(X, Pet).
-- ?- childsPet(parent, X).
getChildrensPets :: Person -> [[Person]]
getChildrensPets parent = relationToMatrix $ applyQuery childrensPetsRelation query
    where childrensPetsRelation = composeRelation parentsRelation petsRelation 
          query = RI.fromList [Fixed parent, Variable]

-- childOrParent(Person, ChildOrParent) :- parent(Person, X); child(Person, X).
-- child(Child, Parent) :- parent(Parent, Child).
-- ?- childOrParent(person, X).
getChildrenAndParents :: Person -> [[Person]]
getChildrenAndParents person = relationToMatrix $ applyQuery childrenParentsRelation query
    where childrenParentsRelation = combineRelation parentsRelation childrenRelation 
          childrenRelation = permuteRelation (RI.fromList [2, 1]) parentsRelation
          query = RI.fromList [Fixed person, Variable]

-- sibling(Person, Sibling) :- child(Person, X), parent(X, Sibling).
-- child(Child, Parent) :- parent(Parent, Child).
-- ?- sibling(person, X).
getSiblings :: Person -> [[Person]]
getSiblings person = relationToMatrix $ applyQuery siblingsRelation query
    where siblingsRelation = composeRelation childrenRelation parentsRelation
          childrenRelation = permuteRelation (RI.fromList [2, 1]) parentsRelation
          query = RI.fromList [Fixed person, Variable]
