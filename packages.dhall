let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20230105/packages.dhall
        sha256:3e9fbc9ba03e9a1fcfd895f65e2d50ee2f5e86c4cd273f3d5c841b655a0e1bda

let additions =
      { aeson =
        { dependencies =
          [ "aff"
          , "argonaut"
          , "argonaut-codecs"
          , "argonaut-core"
          , "arrays"
          , "bifunctors"
          , "const"
          , "control"
          , "effect"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "foreign-object"
          , "integers"
          , "js-bigints"
          , "lists"
          , "maybe"
          , "mote"
          , "numbers"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "quickcheck"
          , "record"
          , "spec"
          , "strings"
          , "tuples"
          , "typelevel"
          , "typelevel-prelude"
          , "uint"
          , "untagged-union"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-aeson.git"
        , version = "v2.0.0"
        }
      , bignumber =
        { dependencies =
          [ "console"
          , "effect"
          , "either"
          , "exceptions"
          , "functions"
          , "integers"
          , "partial"
          , "prelude"
          , "tuples"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-bignumber"
        , version = "760d11b41ece31b8cdd3c53349c5c2fd48d3ff89"
        }
      , properties =
        { dependencies = [ "prelude", "console" ]
        , repo = "https://github.com/Risto-Stevcev/purescript-properties.git"
        , version = "v0.2.0"
        }
      , lattice =
        { dependencies = [ "prelude", "console", "properties" ]
        , repo = "https://github.com/Risto-Stevcev/purescript-lattice.git"
        , version = "v0.3.0"
        }
      , bytearrays =
        { dependencies =
          [ "aeson"
          , "aff"
          , "arraybuffer-types"
          , "effect"
          , "either"
          , "foldable-traversable"
          , "maybe"
          , "newtype"
          , "prelude"
          , "quickcheck"
          , "quickcheck-laws"
          , "spec"
          , "strings"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-bytearrays"
        , version = "v1.0.0"
        }
      , cardano-plutus-data-schema =
        { dependencies = [ "prelude" ]
        , repo =
            "https://github.com/mlabs-haskell/purescript-cardano-plutus-data-schema"
        , version = "v1.0.0"
        }
      , js-bigints =
        { dependencies = [ "integers", "maybe", "prelude" ]
        , repo = "https://github.com/purescript-contrib/purescript-js-bigints"
        , version = "36a7d8ac75a7230043ae511f3145f9ed130954a9"
        }
      , mote-testplan =
        { dependencies =
          [ "aff"
          , "console"
          , "datetime"
          , "effect"
          , "foldable-traversable"
          , "maybe"
          , "mote"
          , "newtype"
          , "numbers"
          , "ordered-collections"
          , "prelude"
          , "spec"
          , "transformers"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-mote-testplan"
        , version = "v1.0.0"
        }
      , cardano-types =
        { dependencies =
          [ "aeson"
          , "aff"
          , "arraybuffer-types"
          , "arrays"
          , "bifunctors"
          , "bytearrays"
          , "cardano-plutus-data-schema"
          , "cardano-data-lite"
          , "control"
          , "datetime"
          , "effect"
          , "either"
          , "encoding"
          , "exceptions"
          , "foldable-traversable"
          , "foreign-object"
          , "integers"
          , "js-bigints"
          , "lattice"
          , "lists"
          , "literals"
          , "maybe"
          , "monad-logger"
          , "mote"
          , "mote-testplan"
          , "newtype"
          , "nonempty"
          , "nullable"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "profunctor"
          , "profunctor-lenses"
          , "quickcheck"
          , "rationals"
          , "record"
          , "safe-coerce"
          , "spec"
          , "these"
          , "tuples"
          , "typelevel-prelude"
          , "uint"
          , "unfoldable"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-cardano-types.git"
        , version = "v5.0.0"
        }
      , cardano-provider =
        { dependencies =
          [ "aeson"
          , "aff"
          , "aff-promise"
          , "affjax"
          , "argonaut-codecs"
          , "arrays"
          , "bifunctors"
          , "cardano-types"
          , "effect"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "lists"
          , "maybe"
          , "newtype"
          , "ordered-collections"
          , "prelude"
          , "strings"
          , "these"
          , "tuples"
          , "uint"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-cardano-provider"
        , version = "v2.1.0"
        }
      , cardano-key-wallet =
        { dependencies =
          [ "aeson"
          , "aff"
          , "arrays"
          , "cardano-collateral-select"
          , "cardano-message-signing"
          , "cardano-types"
          , "console"
          , "effect"
          , "either"
          , "foldable-traversable"
          , "maybe"
          , "newtype"
          , "ordered-collections"
          , "prelude"
          ]
        , repo =
            "https://github.com/mlabs-haskell/purescript-cardano-key-wallet"
        , version = "v2.0.0"
        }
      , cardano-collateral-select =
        { dependencies =
          [ "arrays"
          , "cardano-types"
          , "console"
          , "effect"
          , "exceptions"
          , "foldable-traversable"
          , "lists"
          , "maybe"
          , "newtype"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "tuples"
          ]
        , repo =
            "https://github.com/mlabs-haskell/purescript-cardano-collateral-select"
        , version = "v1.0.0"
        }
      , cardano-message-signing =
        { dependencies =
          [ "bytearrays"
          , "cardano-types"
          , "console"
          , "effect"
          , "newtype"
          , "prelude"
          ]
        , repo =
            "https://github.com/mlabs-haskell/purescript-cardano-message-signing"
        , version = "v1.0.0"
        }
      , cardano-data-lite =
        { dependencies =
          [ "aeson"
          , "aff"
          , "argonaut"
          , "bifunctors"
          , "bytearrays"
          , "effect"
          , "either"
          , "enums"
          , "maybe"
          , "newtype"
          , "nullable"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "profunctor"
          , "spec"
          , "transformers"
          , "tuples"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-cardano-data-lite"
        , version = "070a1a502472211853099c2566a7e9100a7b1a61"
        }
      , cardano-transaction-builder =
        { dependencies =
          [ "aff"
          , "arrays"
          , "bifunctors"
          , "bytearrays"
          , "cardano-types"
          , "datetime"
          , "effect"
          , "either"
          , "foldable-traversable"
          , "maybe"
          , "mote"
          , "mote-testplan"
          , "newtype"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "profunctor-lenses"
          , "record"
          , "spec"
          , "transformers"
          , "tuples"
          , "uint"
          ]
        , repo =
            "https://github.com/mlabs-haskell/purescript-cardano-transaction-builder"
        , version = "v3.0.0"
        }
      }

in (upstream // additions)
