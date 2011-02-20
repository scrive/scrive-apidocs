module AppViewSampler(
    appViewSamples
) where

import Test.HUnit (assert, assertEqual, assertFailure, Assertion(..))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import AppView
import SamplerHelper
import SampleData
import Misc
import KontraLink
import Kontra

import Data.Maybe
import HSP.XML (cdata)
import qualified Data.ByteString as BS (readFile)
import qualified Data.ByteString.UTF8 as BS (toString)
import System.IO.UTF8 as UTF8 (writeFile)

appViewSamples :: [Test]
appViewSamples = 
                     [testGroup "sample app views" 
                           [
                             testCase "signup page" sampleSignupPageView
                            ,testCase "forgot password page" samplePageForgotPasswordView
                            ,testCase "forgot password confirm page" samplePageForgotPasswordConfirmView
                            ,testCase "login page" samplePageLoginView
                           ]
                     ]


sampleSignupPageView = sampleCompleteView "signup_page" (\templ _ -> signupPageView templ) 

samplePageForgotPasswordView = sampleCompleteView "forgot_password_page" (\templ _ -> pageForgotPassword templ)

samplePageForgotPasswordConfirmView = sampleCompleteView "forgot_password_confirm_page" (\templ _ -> pageForgotPasswordConfirm templ)

samplePageLoginView = sampleCompleteView "login_page" (\_ ctx -> pageLogin ctx Nothing)

sampleCompleteView name action =
  let ctx = aTestCtx{ctxmaybeuser=Nothing}
  in sampleView name (\templ -> pageFromBody' "../public" (ctx{ctxtemplates=templ}) TopNone "kontrakcja" (fmap cdata $ action templ ctx))
     where sampleView name action = sample name "view" (\t -> renderHSPToString (action t)) (UTF8.writeFile)
