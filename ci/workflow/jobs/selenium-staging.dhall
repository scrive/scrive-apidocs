let createWorkflow = ../lib/selenium-workflow.dhall

in
createWorkflow
  { branch = "staging"
  , config-key = "STAGING_SELENIUM_CONFIG"
  , run-flags = "--remote"
  }
