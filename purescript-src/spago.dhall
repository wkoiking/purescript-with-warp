{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "profunctor-lenses"
    , "assert"
    , "console"
    , "effect"
    , "lists"
    , "oak"
    , "psci-support"
    , "generics-rep"
    , "aff"
    , "affjax"
    , "argonaut-generic"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
