module RelationQuery
    ( QueryElement(Fixed, Variable)
    , RelationQuery
    ) where

import RelationItem (RelationItem)


data QueryElement a = Fixed { fixedValue :: a } | Variable deriving (Show, Eq)

type RelationQuery a = RelationItem (QueryElement a)