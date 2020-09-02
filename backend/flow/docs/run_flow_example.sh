#!/usr/bin/bash -e

SCRIPT_NAME="$(basename -s .sh "$(readlink -f "$0")")"
SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"
VENV_DIR="${SCRIPT_DIR}/.venv"

declare -a ARGS=()
while (( $# != 0 )); do
  arg="$1"; shift
  case "$arg" in
    --env-reset)
      rm -rf "${VENV_DIR}"
      ;;
    *)
      ARGS+=("$arg")
      ;;
  esac
done

cleanup()
{
  rm -rf "${VENV_DIR}"
  echo "Cleaning up virtual environment." >&2
}

if [[ ! -d "${VENV_DIR}" ]]; then
  (
    trap cleanup ERR
    echo "Creating virtual environment for the first time."
    rm -rf "${VENV_DIR}"
    env python3 -m venv --copies "${VENV_DIR}"
    source "${VENV_DIR}/bin/activate"
    pip3 install -r "${SCRIPT_DIR}/requirements.txt"
    deactivate
    echo "Virtual environment created."
    echo
  )
fi

source "${VENV_DIR}/bin/activate"

"${SCRIPT_DIR}"/flow_example.py "${ARGS[@]}"
