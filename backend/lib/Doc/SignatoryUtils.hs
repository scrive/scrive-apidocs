module Doc.SignatoryUtils (
  shouldForceEmailLink
  ) where

import Doc.Types.SignatoryLink

shouldForceEmailLink :: SignatoryLink -> Bool
shouldForceEmailLink sl = case signatorylinkconfirmationdeliverymethod sl of
  NoConfirmationDelivery                 -> False
  EmailConfirmationDelivery              -> False
  EmailLinkConfirmationDelivery          -> True
  MobileConfirmationDelivery             -> False
  EmailAndMobileConfirmationDelivery     -> False
  EmailLinkAndMobileConfirmationDelivery -> True
