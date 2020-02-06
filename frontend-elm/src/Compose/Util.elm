module Compose.Util exposing (apply, elimEither, joinMaybe, liftCmd, msgToCmd, neither, outerLiftCmd)

import Either exposing (Either(..))
import Task


apply : (a -> b) -> a -> b
apply f x =
    f x


neither : Either Never Never -> a
neither x1 =
    case x1 of
        Left x2 ->
            never x2

        Right x2 ->
            never x2


elimEither : Either Never a -> a
elimEither x1 =
    case x1 of
        Left x2 ->
            never x2

        Right x2 ->
            x2


joinMaybe : Maybe (Maybe a) -> Maybe a
joinMaybe =
    Maybe.andThen identity


liftCmd : (a -> b) -> Cmd (Either c a) -> Cmd (Either c b)
liftCmd =
    Cmd.map << Either.map


outerLiftCmd : (a -> b) -> Cmd (Either a c) -> Cmd (Either b c)
outerLiftCmd =
    Cmd.map << Either.mapLeft


msgToCmd : msg -> Cmd msg
msgToCmd msg =
    Task.perform identity <| Task.succeed msg
