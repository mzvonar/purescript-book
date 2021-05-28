{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "foldable-traversable"
  , "integers"
  , "math"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "strings"
  , "test-unit"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
