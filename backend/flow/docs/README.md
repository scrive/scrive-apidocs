# Flow documentation

This directory contains some documentation about and a script for testing Scrive Flow.

## Python script

This is a self-contained script that sets up and executes a complete Flow process, including
document signing.

To use it, you will need to have an account at `https://dev.scrive.com`.
If you have one, please update fields `user_email` and `user_password` in `flow_example.py`
with your actual credentials.

Once configured, simply run `run_flow_example.sh`, which should take care of setting up a python
virtual environment and running the python script above.

Feel free to modify the code and see how the Flow API reacts to your changes.

The script also contains some inline documentation in the comments.

Please let us know, if you encounter any issues.

## API guide

File `flow-api-guide.md` explains basic Flow concepts and walks you through
setting up of a simple Flow process via the API using the `curl` command in shell.

## Planned API changes

In `flow-api-changes.md` you will find planned changes to the API.
