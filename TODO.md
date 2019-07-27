TODO
====

A non-exhaustive list of things TO DO for Extrapolate.

* improve performance by improving performance of Speculate

* `deriveGeneralizableFromTo`: derive a generalizable value using a bijection
  passed as parameter so that a data invariant is not broken.  Example:

    deriveGeneralizable ''Map fromList toList.

* `add-th-eg`:
  add the parser example from the Feat paper;

* `improve-record-printing`:
  Improve the record printing by explictly printing records and _not_ showing
  variables.  For example, when testing `prop_delete`, currently we get:

	StackSet (Screen (Workspace x y (Just s)) z x’) ss ws crs

  but we could get the following instead (with indent):

    > check prop_delete
    StackSet { current  = Screen
                        { workspace = Workspace
                                    { tag    = x
                                    , layout = y
                                    , stack  = Just s
                                    }
                        , screen       = z
                        , screenDetail = x'
                        }
             , visible  = ss
             , hidden   = ws
             , floating = crs
             }

  which could be further summarized to:

	stackset {current = scr {workspace = ws {stack = Just s}}}
