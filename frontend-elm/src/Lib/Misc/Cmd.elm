module Lib.Misc.Cmd exposing (..)

import Task

-- Someone come up with a better name...
perform : msg -> Cmd msg
perform =
    Task.perform identity << Task.succeed
