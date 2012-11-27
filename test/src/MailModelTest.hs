module MailModelTest (mailModelTests) where

import Test.Framework

import DB
import TestingUtil
import TestKontra as T
import MinutesTime
import Mails.Model
import MagicHash

mailModelTests :: TestEnvSt -> Test
mailModelTests env  = testGroup "Mail Model" [
    testThat "Mail stores attachments properly" env $ testMailAttachments
  ]

testMailAttachments :: TestEnv ()
testMailAttachments = do
  let token = unsafeMagicHash 12341234
      sender = Address "Jarek" "xx@asd.com"
      to = [Address "Genowefa""a1@ss.com", Address "Brunhilda" "b3@dd.com"]
      to_be_sent = fromSeconds 213123
      attachments = [ Attachment "name1" "content 123"
                    , Attachment "name2" "contenty 314124"
                    , Attachment "name3" "contenty 314124 sffadsfa"
                    ]
      xsmtpapi = XSMTPAttrs [("arg1","val1"),("arg2","val2"),("arg3","val3"),("arg4","val4")]
      title = "The thing"
      content = "important"

  mid <- dbUpdate $ CreateEmail token sender to to_be_sent
  _ <- dbUpdate $ AddContentToEmail mid title content attachments xsmtpapi
  mmail <- dbQuery $ GetEmail mid token
  assertJust mmail
  case mmail of
    Nothing -> return ()
    Just mail -> do
      assertEqual "Titles must match" (title) (mailTitle mail)
      assertEqual "Sender must match" (sender) (mailFrom mail)
      assertEqual "To must match" (to) (mailTo mail)
      assertEqual "Attachments must match" (attachments) (mailAttachments mail)
      assertEqual "XSMTPAttrs must match" (xsmtpapi) (mailXSMTPAttrs mail)
      assertEqual "Content must match" (content) (mailContent mail)
