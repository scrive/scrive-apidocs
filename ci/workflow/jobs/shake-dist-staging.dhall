let createWorkflow = ../lib/shake-dist-workflow.dhall
in
createWorkflow
  { name = "staging"
  , branches = [ "staging" ]
  , nginx-rules-path = "nginx/nginx_rules_prod.json"
  , nginx-rules-path-alternative = "nginx/nginx_rules_prod_list.json"
  , nginx-default-rule = "include /etc/nginx/includes/kontrakcja-prod;"
  }
