module SSO.Guards(guardConditionsAreMet, guardAssertionsConditionsAreMet) where

import Control.Monad.Base
import Control.Monad.Catch
import Data.Time.Clock
import SAML2.Core.Assertions
import SAML2.Core.Datatypes

import API.V2.Errors
import API.V2.MonadUtils

guardAssertionsConditionsAreMet
  :: (MonadBase IO m, MonadThrow m) => AnyURI -> (() -> m UTCTime) -> [Assertion] -> m ()
guardAssertionsConditionsAreMet audienceURI nowF assertions = forM_
  assertions
  guardAssertionConditions
  where
    guardAssertionConditions assertion = do
      case assertionConditions assertion of
        Just conditions -> guardConditionsAreMet audienceURI nowF conditions
        Nothing ->
          authErr
            "Too weak SAML provided. Please provide one with at least notBefore, notOnAndAfter and audience conditions."

guardConditionsAreMet
  :: (MonadBase IO m, MonadThrow m) => AnyURI -> (() -> m UTCTime) -> Conditions -> m ()
guardConditionsAreMet audienceURI nowF Conditions { conditionsNotBefore = mNotBefore, conditionsNotOnOrAfter = mNotOnOrAfter, conditions }
  = do
    currentTime <- nowF ()
    maybe
      (authErr "NotOnOrAfter condition not passed")
      (\notOnOrAfter -> when (currentTime >= notOnOrAfter)
        $ authErr ("Assertion used too late " <> showt notOnOrAfter)
      )
      mNotOnOrAfter
    maybe
      (authErr "NotBefore condition not passed")
      (\notBefore -> when (currentTime < notBefore)
        $ authErr ("Assertion used too early " <> showt notBefore)
      )
      mNotBefore
    unless (any isMatchingAudience conditions)
      $ authErr ("No matching audience condition in assertion" <> showt conditions)
  where
    isMatchingAudience :: Condition -> Bool
    isMatchingAudience (AudienceRestriction restriction) =
      Audience { audience = audienceURI } `elem` restriction
    isMatchingAudience _ = False


authErr :: MonadThrow m => Text -> m ()
authErr = apiError . invalidAuthorizationWithMsg
