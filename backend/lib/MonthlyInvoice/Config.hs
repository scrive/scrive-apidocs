module MonthlyInvoice.Config where

import Data.Unjson

data MonthlyInvoiceConf = MonthlyInvoiceConf {
    scriptPath     :: !Text
  , recipientName  :: !Text
  , recipientEmail :: !Text
} deriving (Eq, Show)

unjsonMonthlyInvoiceConf :: UnjsonDef MonthlyInvoiceConf
unjsonMonthlyInvoiceConf =
  objectOf
    $   pure MonthlyInvoiceConf
    <*> field "script_path"     scriptPath     "Path to the .sql script on the disk"
    <*> field "recipient_name"  recipientName  "Report recipient's name"
    <*> field "recipient_email" recipientEmail "Report recipient's email"

instance Unjson MonthlyInvoiceConf where
  unjsonDef = unjsonMonthlyInvoiceConf
