module Flow.Message
  ( Message
  , fromMessage
  , unsafeMessage
  ) where

import Flow.Message.Internal

fromMessage :: Message -> Text
fromMessage (Message m) = m

unsafeMessage :: Text -> Message
unsafeMessage = Message
