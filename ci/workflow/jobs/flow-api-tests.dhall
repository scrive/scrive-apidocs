let createWorkflow = ../lib/flow-api-tests-workflow.dhall

in
createWorkflow
  {
  run-flags = "--capture=sys --env=dev"
  }
