let createWorkflow = ../lib/selenium-scripts-update-workflow.dhall

in
createWorkflow
  {
  , config-key = "STAGING_SELENIUM_CONFIG"
  , run-flags = "--local --timeout 120"
  }
