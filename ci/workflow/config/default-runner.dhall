let config = ../config.dhall
let Job = ../type/Job.dhall

in
if config.self-hosted
then
  [ "linux"
  , "self-hosted"

  -- The organization runner by default has a build-runner label.
  -- To run the builds on custom runners, change the label here.
  , "build-runner"
  ]
else
  [ "ubuntu-20.04" ]
