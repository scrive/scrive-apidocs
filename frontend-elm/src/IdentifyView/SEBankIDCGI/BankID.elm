module IdentifyView.SEBankIDCGI.BankID exposing (..)

import Enum exposing (Enum)

type BankIdState =
    BankIdComplete
  | BankIdUserSign
  -- ^ "Enter your security code in the BankId App and select Sign."
  | BankIdOutstandingTransaction
  -- ^ "Start your BankId App." / "Trying to start your BankId App. You can also
  -- start your app manually on your mobile or on another device."
  | BankIdNoClient
  -- ^ "Start your BankId App."
  | BankIdStarted
  -- ^ "You do not have a BankId that can be used for this signature on this
  -- device. If you don’t have a BankId you can order one from your bank. If you
  -- have a BankId on another device you can start the BankId App on that
  -- device."

enumBankIdState : Enum BankIdState
enumBankIdState =
  let allValues = [ BankIdComplete, BankIdUserSign, BankIdOutstandingTransaction
        , BankIdNoClient, BankIdStarted ]

      toString err = case err of
        BankIdComplete -> "complete"
        BankIdUserSign -> "user_sign"
        BankIdOutstandingTransaction -> "outstanding_transaction"
        BankIdNoClient -> "no_client"
        BankIdStarted -> "started"

  in Enum.makeEnum allValues toString

type BankIdError =
    BankIdInvalidParameters
  | BankIdAlreadyInProgress
  | BankIdAccessDeniedRP
  | BankIdRetry
  | BankIdInternalError
  | BankIdExpiredTransaction
  | BankIdUserCancel
  | BankIdClientErr
  | BankIdCertificateErr
  | BankIdCancelled
  | BankIdStartFailed

enumBankIdError : Enum BankIdError
enumBankIdError =
  let allValues = [ BankIdInvalidParameters, BankIdAlreadyInProgress
        , BankIdAccessDeniedRP, BankIdRetry, BankIdInternalError
        , BankIdExpiredTransaction, BankIdUserCancel, BankIdClientErr
        , BankIdCertificateErr, BankIdCancelled, BankIdStartFailed ]

      toString err = case err of
        BankIdInvalidParameters -> "invalid_parameters"
        BankIdAlreadyInProgress -> "already_in_progress"
        BankIdAccessDeniedRP -> "access_denied_rp"
        BankIdRetry -> "retry"
        BankIdInternalError -> "internal_error"
        BankIdExpiredTransaction -> "expired_transaction"
        BankIdUserCancel -> "user_cancel"
        BankIdClientErr -> "client_error"
        BankIdCertificateErr -> "certificate_error"
        BankIdCancelled -> "cancelled"
        BankIdStartFailed -> "start_failed"

  in Enum.makeEnum allValues toString

-- | Helper that returns a 12 digit personal number (without separators).
normalisePersonalNumber : {currentYear : Int, rawPersonalNumber : String} -> Maybe String
normalisePersonalNumber {currentYear, rawPersonalNumber} =
  let clean = String.filter Char.isDigit rawPersonalNumber -- 10 or 12 digits, no separators
  in
  if String.length clean == 12
    then Just clean
    else
      -- We need to prefix 10 digit personal numbers with "19" or "20".
      let centenarian         = String.contains "+" rawPersonalNumber
          -- ^ See https://en.wikipedia.org/wiki/Personal_identity_number_(Sweden)#Format.
      in
      String.toInt (String.left 2 clean)
      |> Maybe.map
          (\yy -> if currentYear - (1900 + yy) < 100
                  then "19" ++ clean
                  else if centenarian
                    then "19" ++ clean
                    else "20" ++ clean
          )
