TODO
====

A non-exhaustive list of things TO DO for Extrapolate.

* simplify test handling `make test/x.run`

* `deriveGeneralizable` should `deriveListableIfNeeded`.

* `deriveGeneralizableAtomic`: derive a generalizable value treating it as
  atomic, as I did with the Map on the XMonad example.  To be used on datatypes
  with data invariants.

* `deriveGeneralizableFromTo`: derive a generalizable value using a bijection
  passed as parameter so that a data invariant is not broken.  Example:

    deriveGeneralizable ''Map fromList toList.

* `background-exclusion`:
  add a mechanism to exclude functions from the background.

* `report-multiple-generalizations`:
  when there is more than one generalization and they don't encompass
  one-another, report both.

* `renaming`:
  possibly print          `(Div (C 0) (Add (C x) (C (negate x))))`
  which is equivalent to  `(Div (C 0) (Add (C x) (C y))) when y == negate x`
  instead of              `(Div (C 0) (Add (C 0) (C 0)))`;

  I got it to print the  middle one, by just: `-- not (isAssignment wc)` and
  `constant "negate" (negate -:> x) in the background`.

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


performance and improvements in the algorithm (only later)
----------------------------------------------------------

* _type-after-type_:
  to improve performance, instead of working with all types at once, perform
  the algorithm type after type

* _single-then-multi_:
  only do canonicalVariations *after* finding a failing single variable
  instance.  I'll have to re-test, but the time I save may pay off.
