let NixShell =
  < dev-shell | dev-shell-optimized | manual-shell >

let format = \(shell: NixShell) ->
    merge
      { dev-shell = "dev-shell"
      , dev-shell-optimized = "dev-shell-optimized"
      , manual-shell = "manual-shell"
      }
      shell
in
{ Type = NixShell
, format = format
}
