module SSO.Guards(guardConditionsAreMet, guardAssertionsConditionsAreMet) where

import Control.Monad.Base
import Control.Monad.Catch
import Data.Time.Clock
import SAML2.Core.Assertions
import SAML2.Core.Datatypes

import KontraError

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
          unauthorized
            "Too weak SAML provided. Please provide one with at least notBefore, notOnAndAfter and audience conditions."

guardConditionsAreMet
  :: (MonadBase IO m, MonadThrow m) => AnyURI -> (() -> m UTCTime) -> Conditions -> m ()
guardConditionsAreMet audienceURI nowF Conditions { conditionsNotBefore = mNotBefore, conditionsNotOnOrAfter = mNotOnOrAfter, conditions }
  = do
    currentTime <- nowF ()
    maybe
      (unauthorized "NotOnOrAfter condition not passed")
      (\notOnOrAfter -> when (currentTime >= notOnOrAfter)
        $ unauthorized ("Assertion used too late " <> showt notOnOrAfter)
      )
      mNotOnOrAfter
    maybe
      (unauthorized "NotBefore condition not passed")
      (\notBefore -> when (currentTime < notBefore)
        $ unauthorized ("Assertion used too early " <> showt notBefore)
      )
      mNotBefore
    unless (any isMatchingAudience conditions)
      $ unauthorized ("No matching audience condition in assertion" <> showt conditions)
  where
    isMatchingAudience :: Condition -> Bool
    isMatchingAudience (AudienceRestriction restriction) =
      Audience { audience = audienceURI } `elem` restriction
    isMatchingAudience _ = False
