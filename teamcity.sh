#!/bin/bash

set -o errexit
set -o nounset

readonly current_commit=${BUILD_VCS_NUMBER:-$(git rev-parse HEAD)};
readonly teamcity_build_id=$(sed -n -e 's/^teamcity.build.id=\(.*\)$/\1/p' ${TEAMCITY_BUILD_PROPERTIES_FILE:-/dev/null})
teamcity_build_url="";
exit_code=0;

if [ -z "${teamcity_build_id}" ]; then
    readonly teamcity_build_url="";
else
    readonly teamcity_build_url="http://builds.scrive.lan/viewLog.html?buildId=${teamcity_build_id}&tab=buildLog";
fi

function main()
{
    while [ $# -ne 0 ]; do

        case $1 in
            check-import-order)
                step_check_import_order;
                ;;
            validate-js)
                step_validate_js;
                ;;
            *)
                echo "Unknown command '$1', ignoring";
                ;;
        esac;

        shift;
    done
}

function step_check_import_order()
{
    github_notify_commit_state "check-import-order" "pending" "Checking Haskell imports";
    if scripts/sort_imports.sh --check; then
        github_notify_commit_state "check-import-order" "success" "Haskell imports in proper order";
        exit_code=0;
    else
        github_notify_commit_state "check-import-order" "error" "Some Haskell imports are in wrong order";
        exit_code=1;
    fi
}

function step_validate_js()
{
    github_notify_commit_state "validate-js" "pending" "Linting the JS";
    if cd frontend && grunt validateJs; then
        github_notify_commit_state "validate-js" "success" "All files passed";
        exit_code=0;
    else
        github_notify_commit_state "validate-js" "error" "Some files are not well formed according. Run grunt validateJs to find out which.";
        exit_code=1;
    fi
}

function github_notify_commit_state()
{
    local context=$1;
    local state=$2;
    local description=$3;

    if [ -n "${TEAMCITY_VERSION:-}" ]; then

        local tmpname=$(mktemp curl-github-output.XXXXXX)

        # The token belongs to 'scrive-bot@scrive.com' account on
        # github.com. Note that the bot must have at least 'Write'
        # access permission as 'Read' is not enough to push build
        # status notifications.
        #
        # There is no way to make curl return http status code so we
        # go through the strange write-out parameter.
        local http_code=$(curl --silent --location \
                               --output "${tmpname}" \
                               --write-out '%{http_code}' \
                               -XPOST https://api.github.com/repos/scrive/kontrakcja/statuses/${current_commit} \
                               -H 'Content-type: application/json' \
                               -H 'Authorization: token a881167a944db3070d39d2d0551605da90d45644' \
                               -T - <<EOF
{
  "state": "${state}",
  "target_url": "${teamcity_build_url}",
  "description": "${description}",
  "context": "${context}"
}
EOF
              )
        if [[ "${http_code}" =~ ^2.. ]]; then
            true;
        else
            echo "GithubAPI returned ${http_code} HTTP status code" >&2
            cat "${tmpname}" >&2
        fi
        rm "${tmpname}"
    else
        echo "Skipping github notifications: ${state}, ${description}";
    fi
}

main "$@";

exit ${exit_code};
