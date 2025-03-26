{ name = "purescript-cardano-transaction-balancer"
, dependencies =
  [ "aff"
  , "ansi"
  , "arrays"
  , "bifunctors"
  , "bytearrays"
  , "cardano-data-lite"
  , "cardano-kupmios-provider"
  , "cardano-provider"
  , "cardano-transaction-builder"
  , "cardano-types"
  , "console"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "integers"
  , "js-bigints"
  , "js-date"
  , "lattice"
  , "lists"
  , "literals"
  , "maybe"
  , "monad-logger"
  , "newtype"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "quickcheck"
  , "random"
  , "strings"
  , "stringutils"
  , "these"
  , "transformers"
  , "tuples"
  , "uint"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
