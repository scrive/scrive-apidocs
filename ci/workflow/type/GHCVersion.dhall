let GHCVersion = < ghc88 | ghc86 >

let format = \(version: GHCVersion) ->
  merge
    { ghc88 = "ghc88"
    , ghc86 = "ghc86"
    }
    version

in
  { Type = GHCVersion
  , format = format
  }
