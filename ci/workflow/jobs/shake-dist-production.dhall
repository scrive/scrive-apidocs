let createWorkflow = ../lib/shake-dist-workflow.dhall
in
createWorkflow
  { name = "production"
  , branches = [ "production" ]
  , nginx-rules-path = "nginx/nginx_rules_staging.json"
  , nginx-rules-path-alternative = "nginx/nginx_rules_staging_list.json"
  , nginx-default-rule = "include /etc/nginx/includes/kontrakcja-staging;"
  }
