{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "learn-git"
, dependencies =
  [ "aff-promise"
  , "concur-react"
  , "quickcheck"
  , "spec"
  , "spec-quickcheck"
  , "string-parsers"
  ]
, packages = ./packages.dhall
}
