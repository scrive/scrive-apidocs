let createWorkflow = ../lib/shake-dist-workflow.dhall
in
createWorkflow
  { name = "master"
  , branches = [ "master", "nix" ]
  , nginx-rules-path = "nginx/nginx_rules_dev.json"
  , nginx-rules-path-alternative = ""
  , nginx-default-rule = "include /etc/nginx/includes/kontrakcja-dev;"
  }
