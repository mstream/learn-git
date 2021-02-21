{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "learn-git"
, dependencies =
  [ "aff-promise"
  , "arraybuffer-types"
  , "concur-react"
  , "motsunabe"
  , "node-buffer"
  , "quickcheck"
  , "spec"
  , "spec-quickcheck"
  , "string-parsers"
  ]
, packages = ./packages.dhall
}
