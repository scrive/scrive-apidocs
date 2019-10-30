module EvidenceLogTest (evidenceLogTests) where

import Data.Int
import Data.Set (Set)
import Test.Framework
import Text.StringTemplate (checkTemplateDeep, getStringTemplate)
import Text.StringTemplates.Templates
import qualified Data.Set as Set
import qualified Data.Text as T

import DB
import EvidenceLog.Model
import EvidenceLog.View (historyEventType)
import TestingUtil
import TestKontra
import User.Lang (allLangs, codeFromLang)

evidenceLogTests :: TestEnvSt -> Test
evidenceLogTests env = testGroup
  "Evidence Log"
  [ testThat "Testing EvidenceEventType conversions equal" env conversionEq
  , testThat
    (  "Testing EvidenceEventType templates are well defined and "
    <> "their variable set is known"
    )
    env
    evidenceLogTemplatesWellDefined
  ]

evidenceEventTypes :: [(Int16, EvidenceEventType)]
evidenceEventTypes = zip [1 ..]
  $ concat [map Current [minBound .. maxBound], map Obsolete [minBound .. maxBound]]

conversionEq :: TestEnv ()
conversionEq = do
  let eventsNum = fromIntegral (length evidenceEventTypes) :: Int16
  forM_ evidenceEventTypes $ \(n, t) -> do
    -- test that fromSQL . toSQL == id (fromSQL is onto and toSQL is
    -- bijection as function EvidenceEventType -> {1..eventsNum})
    runQuery_ $ rawSQL "SELECT $1, $1 >= 1 AND $1 <= $2" (t, eventsNum)
    (t', True) <- fetchOne identity
    assertBool ("fromSQL . toSQL /= id on " ++ show t) $ t' == t

    -- test that id . toSQL . fromSQL == id (fromSQL|{1..eventsNum} is
    -- a bijection)
    runSQL_ $ "SELECT" <+> unsafeSQL (show n) <+> "::int2"
    tt :: EvidenceEventType <- fetchOne runIdentity
    runSQL_ $ "SELECT" <?> tt
    n' <- fetchOne runIdentity
    assertBool ("toSQL . fromSQL /= id on " ++ show n) $ n' == n

evidenceLogTemplateVariables :: Set String
evidenceLogTemplateVariables = Set.fromList
  [ "actor"           -- Person identifier (possibly prefixed by role)
  , "checked"         -- Field updates :: Bool
  , "description"     -- Attachment request description
  , "email"           -- Bool
  , "fieldname"       -- Field updates
  , "fieldvalues"     -- List of radiogroup selectable options
  , "lang"            -- Pending :: Lang
  , "mobile"          -- Flag
  , "name"            -- Attachment name
  , "newblank"        -- Field updates :: Bool
  , "newemail"        -- Undelivery updates
  , "newphone"        -- Undelivery updates
  , "oldemail"        -- Undelivery updates
  , "oldphone"        -- Undelivery updates
  , "phone"           -- Document signed, SMS identification
  , "placements"      -- Field updates :: [(Int, Double, Double)]
  , "prvblank"        -- Field updates :: Bool
  , "signatory"       -- Person identifier
  , "signatory_email" -- Invitation read
  , "signing"         -- Flag
  , "sms_pin"         -- Flag
  , "text"            -- Messages :: String
  , "timeouttime"     -- Pending :: UTCTime
  , "timezone"        -- Pending :: TimeZone
  , "value"           -- Field updates :: FieldValue
  , "provider_sebankid"  -- Provider is Swedish BankID
  , "provider_sms_pin"  -- Indetified with PIN by SMS
  , "hide_pn"         -- whether personal number should be hidden
  , "attachment_name" -- Name of attachment when accepting authors attachments
  , "attachment_acceptance_text" -- Text that should have been shown to user when rendering sign view and accepting attachment
  , "attachment_nothing_to_upload_text" -- Text that should have been shown to user when rendering sign view and choosing not to upload attachment
  , "hash"            -- SHA256 hash of attachment that will not be appended to main, sealsed file
  , "pageno"          -- Number of page affected by event. Used when setting highlighting
  , "question"          -- Consent question's title
  , "response"          -- Consent question's response
  , "description_title" -- Consent question's detailed description title
  , "description_text"  -- Consent question's detailed description text
  , "emailmobile"       -- This relates to the bug found by MR and described in evidenceLogTemplatesWellDefined definition.
  , "used_email_address" -- Email address which was used to invite (for use in evidence log)
  , "used_mobile_number" -- SMS telnum which was used to invite (for use in evidence log)
  ]

-- Attachment name                                       , "name"
-- Attachment request description                        , "description"
-- Bool                                                  , "email"
-- Document signed, SMS identification                   , "phone"
-- Field updates                                         , "fieldname"
-- Field updates :: [(Int, Double, Double)]              , "placements"
-- Field updates :: Bool                                 , "checked"
-- Field updates :: Bool                                 , "newblank"
-- Field updates :: Bool                                 , "prvblank"
-- Field updates :: FieldValue                           , "value"
-- Flag                                                  , "mobile"
-- Flag                                                  , "signing"
-- Flag                                                  , "sms_pin"
-- Invitation read                                       , "signatory_email"
-- Messages :: HTML                                      , "text"
-- Pending :: Lang                                       , "lang"
-- Pending :: TimeZone                                   , "timezone"
-- Pending :: UTCTime                                    , "timeouttime"
-- Person identifier                                     , "author"
-- Person identifier                                     , "signatory"
-- Person identifier (possibly prefixed by role)         [ "actor"
-- Undelivery updates                                    , "newemail"
-- Undelivery updates                                    , "newphone"
-- Undelivery updates                                    , "oldemail"
-- Undelivery updates                                    , "oldphone"

evidenceLogTemplatesWellDefined :: TestEnv ()
evidenceLogTemplatesWellDefined = do
  vars <-
    fmap (Set.fromList . concat)
    $ forM
        [ (ty, l, ta)
        | (_, ty) <- evidenceEventTypes
        , l       <- allLangs
        , ta      <- [minBound .. maxBound]
        , (ta == EventForEvidenceLog) || (ta == EventForArchive && historyEventType ty)
        ]
    $ \(ty, l, ta) -> do
        case ty of
          Obsolete _  -> return []
          Current  ct -> do
            ts <- getTextTemplatesByLanguage $ T.unpack $ codeFromLang l
            let tn = eventTextTemplateName ta ct
            case getStringTemplate (T.unpack tn) ts of
              Nothing -> do
                assertFailure $ "Cannot find template name " ++ show tn
                return []
              Just st -> do
                -- NOTE: checkTemplateDeep in HStringTemplates 8.3 is not
                -- reliable. It can miss fields when ($if$) is used. Bug
                -- reported by MR.
                let (pe, freevars, te') = checkTemplateDeep st
                let te                  = filter (/= "noescape") te'
                let
                  errcontext =
                    " in template " ++ (T.unpack tn) ++ " for language " ++ show l ++ ": "
                unless (null pe) $ do
                  assertFailure $ "Parse error" ++ errcontext ++ show pe
                unless (null te) $ do
                  assertFailure $ "Unknown template function" ++ errcontext ++ show te
                return freevars
  -- If this test fails please first check note above
  assertEqual
    (  "Evidence log templates has variables not defined in "
    <> "evidenceLogTemplateVariables"
    )
    Set.empty
    (vars Set.\\ evidenceLogTemplateVariables)
  assertEqual
    ("evidenceLogTemplateVariables has elements not used in " <> "evidence log templates")
    Set.empty
    (evidenceLogTemplateVariables Set.\\ vars)
