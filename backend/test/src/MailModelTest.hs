module MailModelTest (mailModelTests) where

import Test.Framework

import DB
import MagicHash
import Mails.Model
import TestingUtil
import TestKontra

mailModelTests :: TestEnvSt -> Test
mailModelTests env = testGroup
  "Mail Model"
  [testThat "Mail stores attachments properly" env $ testMailAttachments]

testMailAttachments :: TestEnv ()
testMailAttachments = do
  let token    = unsafeMagicHash 12341234
      sender   = Address "Jarek" "xx@asd.com"
      to       = [Address "Genowefa" "a1@ss.com", Address "Brunhilda" "b3@dd.com"]
      reply_to = Just (Address "Luluka" "k2@ba.uk")
      attachments =
        [ Attachment "name1" (Left "content 123")
        , Attachment "name2" (Left "contenty 314124")
        , Attachment "name3" (Left "contenty 314124 sffadsfa")
        ]
      title   = "The thing"
      content = "important"

  mid <- dbUpdate $ CreateEmail (token, sender, to, reply_to, title, content, attachments)
  mmail <- dbQuery $ GetEmail mid token
  assertJust mmail
  case mmail of
    Nothing   -> return ()
    Just mail -> do
      assertEqual "Titles must match"      (title)       (mailTitle mail)
      assertEqual "Sender must match"      (sender)      (mailFrom mail)
      assertEqual "Reply-To must match"    (reply_to)    (mailReplyTo mail)
      assertEqual "To must match"          (to)          (mailTo mail)
      assertEqual "Attachments must match" (attachments) (mailAttachments mail)
      assertEqual "Content must match"     (content)     (mailContent mail)
