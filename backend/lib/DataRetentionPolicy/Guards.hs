module DataRetentionPolicy.Guards
  ( guardThatDataRetentionPolicyIsValid
  ) where

import API.V2
import DataRetentionPolicy
import Doc.Types.DocumentStatus
import Kontra

-- | Check that the data retention policy is valid. If another DRP is given,
-- check that the first one is stricter or equivalent, i.e. number of days are
-- lower than or equal to that of the second DRP.
guardThatDataRetentionPolicyIsValid
  :: Kontrakcja m => DataRetentionPolicy -> Maybe DataRetentionPolicy -> m ()
guardThatDataRetentionPolicyIsValid drp mParentDRP = do
  let limits =
        [ (Preparation  , "idle_doc_timeout_preparation")
        , (Closed       , "idle_doc_timeout_closed")
        , (Canceled     , "idle_doc_timeout_canceled")
        , (Timedout     , "idle_doc_timeout_timedout")
        , (Rejected     , "idle_doc_timeout_rejected")
        , (DocumentError, "idle_doc_timeout_error")
        ]
  forM_ limits $ \(status, param) -> do
    let limit = fromMaybe 365 $ mParentDRP >>= view (drpIdleDocTimeout status)
    case drp ^. drpIdleDocTimeout status of
      Just value | value <= 0 || value > limit ->
        apiError . requestParameterInvalid param $ "must be between 1 and" <+> showt limit
      _ -> return ()

  when
      ((view #immediateTrash <$> mParentDRP) == Just True && not (drp ^. #immediateTrash))
    . apiError
    $ requestParameterInvalid "immediate_trash"
                              "must be selected as company has selected it"
