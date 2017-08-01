TODO
====

A non-exhaustive list of things TO DO for Extrapolate.


bugs
----

* `stack-derivation`:
  the current derivation mechanism cannot handle the following datatype:

    data Stack a = Stack a (Stack a) | Empty

  Change it to use `asTypeOf` instead of `argTypes0`.

* `simplify-derived-instances`:
  it seems to me that `argTypesN` is too complicated and unreliable and there
  is a simpler solution.  Maybe using sommething like `stack-derivation` but
  for all other arities of constructors.  For example, I don't think
  `argTypesN` can handle the `Either` type.  Test and see.


examples
--------

* `add-redblack-eg`:
  add the redblacktree example from Small/SmartCheck/Okasaki.
  see `ideal-generalization`.
  Also described on: http://matt.might.net/articles/quick-quickcheck/

  It has been added, but cannot generalize the counter-examples found:  during
  generalization, the very exceptions that cause the error, make the
  generalization fail.  Somehow catch those errors on generalization.

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

* `add-derive-tests`:
  add tests of derivation;

* `show-and-expr-display`:
  display counterExamples both as Show and Expr, I don't know how easy is to
  use show since counterExamples are represented as Exprs, I don't think I can
  eval because I am not bound in a type context.

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

* `no-listable-tuples`:
  there is no need to include listable tuples in the instances list (or at
  least no need to use it).  Instead of having `xy` we would get `(x,y)`.
  However, maybe it is fine as it is.

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
