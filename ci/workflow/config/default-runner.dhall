let config = ../config.dhall
let Job = ../type/Job.dhall

in
if config.self-hosted
then
  [ "linux"
  , "self-hosted"
  ]
else
  [ "ubuntu-latest" ]
