let GHCVersion = < ghc88 | ghc810 >

let format = \(version: GHCVersion) ->
  merge
    { ghc88 = "ghc88"
    , ghc810 = "ghc810"
    }
    version

in
  { Type = GHCVersion
  , format = format
  }
