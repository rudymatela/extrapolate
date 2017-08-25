TODO
====

A non-exhaustive list of things TO DO for Extrapolate.


examples
--------

* `ideal-generalizations`:
  Add examples of ideal generalizations as described in past paper by human
  experts.  The counter example is `blah` because the property fails for every
  `bleh` and `blih`.  I found:

  - one on Duregard's licentiate Thesis:  `prop_cycle` from `BNFC-meta`.
  - one on Duregard's doctorate Thesis.
  - one on SmallCheck's paper: `prop_insertRB` from RedBlack

  there are none on:

  - QuickCheck
  - Testing and Tracing with Quickcheck and Hat
  - SmartCheck
  - Feat
  - Real World Haskell
  - Learn-you-a-haskell
  - Growing and shrinking polygons
  - QuickFuzz

* `add-th-eg`:
  add the parser example from the Feat paper;

* `apply-lazysmallcheck`:
  apply Lazy SmallCheck to calculator and parse and record the results.

feature
-------

* `report-multiple-generalizations`:
  when there is more than one generalization and they don't encompass
  one-another, report both.

* `detect-silly-conditionals`:
  eg:

    xs  when  0 /= length xs

  that's just:

    _:_

  the above is for the last property of the list example

* `renaming`:
  possibly print          `(Div (C 0) (Add (C x) (C (negate x))))`
  which is equivalent to  `(Div (C 0) (Add (C x) (C y))) when y == negate x`
  instead of              `(Div (C 0) (Add (C 0) (C 0)))`;

  I got it to print the  middle one, by just: `-- not (isAssignment wc)` and
  `constant "negate" (negate -:> x) in the background`.


* `improve-record-printing`:
  Improve the record printing by explictly printing records and _not_ showing
  variables.  For example, when testing `prop_delete`, currently we get:

	StackSet (Screen (Workspace x y (Just s)) z x’) ss ws crs

  but we could get instead, with the actual following indentation:

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

* `type-after-type`:
  to improve performance, instead of working with all types at once, perform
  the algorithm type after type

* `single-then-multi`:
  only do vassignments *after* finding a failing single variable instance.
  I'll have to re-test, but the time I save may pay off.

* `new-lgg-algorithm`:

  1. test and keep all tests that pass and fail

  2. pick first counter-example, note that it does not match any of the passing
     tests

  3. first counter-example is current generalization

  4. compute lgg of current generalization with the next counter-example

  5. if it does not matches any of the passing tests, update current generalization

  6. go to 4

  7. generalize to several variables

  Variation of steps 4 and 5 with conditions:

  4. compute lgg of current generaliation with the next counter-example,
     find weakest condition for it to hold

  5. if lgg matches 10% of failing tests, update current generalization.

  The variation may have trouble when dealing with multiple variables,
  maybe there should be a multi-lgg, or start with all vars different



won't fix
---------

* `nlp-example`:
  add the NLP example from the SmartCheck paper.  I don't think I'll use this
  because of a few issues.

  GenI has the bit-rot:

  - GenI 0.24.3 refuses to build on GHC 8.0.1;

  - GenI 0.24.3 builds on GHC 7.8 but test files are missing;

  - The version on git is the most up-to-date contradicting the fact that it is
	supposed to be a mirror from Darcs.  It does not seem to be building on
	Travis.  It has tests, but I have not tried compiling it.

  Lee Pike's paper does not list the exact property and fault for which
  SmartCheck recuces its counterexample.  I also looked at the TeX comments,
  and the info is also not there.  The mentioned stackoverflow question does
  not help with that either.  I could certainly ask of course if I choose to
  carry on with this.

  Thinking again, maybe it is a good idea to use this.  It is a real library
  with real bugs in the git history.
