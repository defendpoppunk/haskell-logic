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
parentsRelation = listToRelation parentsList

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
petsRelation = listToRelation petsList

-- ?- parent(parent, child).
isParent :: Person -> Person -> Bool
isParent parent child = relationHasResults $ applyQuery parentsRelation query
    where query = RelQuery [Fixed parent, Fixed child]

-- ?- parent(parent, X).
getChildren :: Person -> [[Person]]
getChildren parent = relationToList $ applyQuery parentsRelation query
    where query = RelQuery [Fixed parent, Variable]

-- ?- parent(X, child).
getParents :: Person -> [[Person]]
getParents child = relationToList $ applyQuery parentsRelation query
    where query = RelQuery [Variable, Fixed child]

-- grandparent(Grandparent, Grandchild) :- parent(Grandparent, X), parent(X, Grandchild).
-- ?- grandparent(sarah, X).
getGrandchildren :: Person -> [[Person]]
getGrandchildren grandparent = relationToList $ applyQuery grandparentsRelation query
    where grandparentsRelation = composeRelation parentsRelation parentsRelation 
          query = RelQuery [Fixed grandparent, Variable]

-- childsPet(Parent, Pet) :- parent(Parent, X), pet(X, Pet).
getChildrensPets :: Person -> [[Person]]
getChildrensPets parent = relationToList $ applyQuery childrensPetsRelation query
    where childrensPetsRelation = composeRelation parentsRelation petsRelation 
          query = RelQuery [Fixed parent, Variable]

-- childOrParent(Person, ChildOrParent) :- parent(Person, X); child(Person, X).
-- child(Child, Parent) :- parent(Parent, Child).
getChildrenAndParents :: Person -> [[Person]]
getChildrenAndParents person = relationToList $ applyQuery childrenParentsRelation query
    where childrenParentsRelation = combineRelation parentsRelation childrenRelation 
          childrenRelation = permuteRelation [2, 1] parentsRelation
          query = RelQuery [Fixed person, Variable]

-- childOrParent(Person, Sibling) :- child(Person, X), parent(X, Sibling).
-- child(Child, Parent) :- parent(Parent, Child).
getSiblings :: Person -> [[Person]]
getSiblings person = relationToList $ applyQuery siblingsRelation query
    where siblingsRelation = composeRelation childrenRelation parentsRelation
          childrenRelation = permuteRelation [2, 1] parentsRelation
          query = RelQuery [Fixed person, Variable]