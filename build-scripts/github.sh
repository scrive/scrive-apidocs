#!/bin/bash
set -o errexit
set -o nounset

readonly current_commit=${BUILD_VCS_NUMBER:-$(git rev-parse HEAD)};
readonly teamcity_build_id=$(sed -n -e 's/^teamcity.build.id=\(.*\)$/\1/p' ${TEAMCITY_BUILD_PROPERTIES_FILE:-/dev/null})
teamcity_build_url="";
if [ -z "${teamcity_build_id}" ]; then
    readonly teamcity_build_url="";
else
    readonly teamcity_build_url="http://builds.scrive.lan/viewLog.html?buildId=${teamcity_build_id}&tab=buildLog";
fi

context=$1;
state=$2;
description=$3;

tmpname=$(mktemp curl-github-output.XXXXXX)

# The token belongs to 'scrive-bot@scrive.com' account on
# github.com. Note that the bot must have at least 'Write'
# access permission as 'Read' is not enough to push build
# status notifications.
#
# There is no way to make curl return http status code so we
# go through the strange write-out parameter.
http_code=$(curl --silent --location \
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
    exit 0
    rm "${tmpname}"
else
    echo "GithubAPI returned ${http_code} HTTP status code" >&2
    cat "${tmpname}" >&2
    exit 1
    rm "${tmpname}"
fi
