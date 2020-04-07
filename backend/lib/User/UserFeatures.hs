module User.UserFeatures (
    getUserFeaturesJSON
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Time
import Data.Aeson
import Data.Unjson

import DB
import FeatureFlags.Model
import User.Model
import UserGroup.FreeDocumentTokens.Model
import UserGroup.Model
import UserGroup.Types
import UserGroup.Types.PaymentPlan

getUserFeaturesJSON
  :: (MonadDB m, MonadBase IO m, MonadThrow m, MonadTime m) => User -> m Value
getUserFeaturesJSON user = do
  ugwp <- dbQuery . UserGroupGetWithParentsByUserID $ user ^. #id
  let pp       = ugwpPaymentPlan ugwp
      features = ugwpFeatures ugwp
  freeTokens      <- dbQuery (UserGroupFreeDocumentTokensGet $ user ^. #groupID)
  freeTokensCount <- numberOfValidTokens freeTokens
  let featureFlags =
        if user ^. #isCompanyAdmin then fAdminUsers features else fRegularUsers features
  return
    .  object
    $  [ "payment_plan" .= unjsonToJSON unjsonDef pp
       , "features" .= unjsonToJSON unjsonDef featureFlags
       ]
    ++ case pp of
         FreePlan ->
           [ "payment_plan_free_data"
               .= object ["free_document_tokens" .= freeTokensCount]
           ]
         _ -> []
