module BrandedDomain.Tables (tableBrandedDomains) where

import Data.Int
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.UTF8 as BS

import DB

tableBrandedDomains :: Table
tableBrandedDomains = tblTable {
  tblName = "branded_domains"
  , tblVersion = 6
  , tblColumns =
    [ tblColumn { colName = "id",                            colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "url",                           colType = TextT, colNullable = False }
    , tblColumn { colName = "sms_originator",                colType = TextT, colNullable = False }
    , tblColumn { colName = "email_originator",              colType = TextT, colNullable = False }
    , tblColumn { colName = "contact_email",                 colType = TextT, colNullable = False }
    , tblColumn { colName = "noreply_email",                 colType = TextT, colNullable = False, colDefault = Just "''::text"}
    , tblColumn { colName = "mail_theme",                    colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "signview_theme",                colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "service_theme",                 colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "login_theme",                   colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "main_domain",                   colType = BoolT, colNullable = False, colDefault = Just "false" }
    , tblColumn { colName = "browser_title",                 colType = TextT, colNullable = False }
    , tblColumn { colName = "favicon",                       colType = BinaryT, colNullable = False  }
    , tblColumn { colName = "participant_color_1",           colType = TextT, colNullable = False }
    , tblColumn { colName = "participant_color_2",           colType = TextT, colNullable = False }
    , tblColumn { colName = "participant_color_3",           colType = TextT, colNullable = False }
    , tblColumn { colName = "participant_color_4",           colType = TextT, colNullable = False }
    , tblColumn { colName = "participant_color_5",           colType = TextT, colNullable = False }
    , tblColumn { colName = "participant_color_6",           colType = TextT, colNullable = False }
    , tblColumn { colName = "draft_color",                   colType = TextT, colNullable = False }
    , tblColumn { colName = "cancelled_color",               colType = TextT, colNullable = False }
    , tblColumn { colName = "initiated_color",               colType = TextT, colNullable = False }
    , tblColumn { colName = "sent_color",                    colType = TextT, colNullable = False }
    , tblColumn { colName = "delivered_color",               colType = TextT, colNullable = False }
    , tblColumn { colName = "opened_color",                  colType = TextT, colNullable = False }
    , tblColumn { colName = "reviewed_color",                colType = TextT, colNullable = False }
    , tblColumn { colName = "signed_color",                  colType = TextT, colNullable = False }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  , tblIndexes = [
      indexOnColumn "url",
      uniqueIndexOnColumnWithCondition "main_domain" "main_domain" -- Constrain to have only one main domain
    ]
  , tblForeignKeys = [
      (fkOnColumn "mail_theme" "themes" "id") {fkOnDelete = ForeignKeyCascade , fkDeferred = True},
      (fkOnColumn "signview_theme" "themes" "id") {fkOnDelete = ForeignKeyCascade, fkDeferred = True },
      (fkOnColumn "service_theme" "themes" "id") {fkOnDelete = ForeignKeyCascade, fkDeferred = True },
      (fkOnColumn "login_theme" "themes" "id") {fkOnDelete = ForeignKeyCascade, fkDeferred = True }
   ]
  , tblInitialSetup = Just $ TableInitialSetup {
      checkInitialSetup = do
        runQuery_ . sqlSelect "branded_domains" $  do
          sqlWhere "main_domain"
          sqlResult "id"
        (mainDomains::[Int64]) <- fetchMany unSingle
        return $ length mainDomains == 1
      ,
      initialSetup = do
        let mailLogo = "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"
        let mainLogo = "iVBORw0KGgoAAAANSUhEUgAAAHgAAAAXCAYAAADAxotdAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAABFlJREFUeNrsWr1O3EAQvouuRfIb4Dfg0iNh3uAq2jhPEPIAKE5LY/IEPp7goEwRmTRpKHxRGjqDUNr4kiIFjbN7GaNlbma9s7Z1ys9IqwN7d7ye//nWo9F/+qtpzN2o6zpQP5EaU2bKlRrL8Xi8kj5U8Q6Bt/7dU+NWje/A7+JfELySQQQy2AU5aBncabkqGVwN+mA1FrU7LWCzrrzzFn6VGgkYmNhwYK1+RqGZGfdSuK5Halxv9pPDnKiD7LDcpsScWI2yRQb6fjyEcpPan5IW3qmQXyUwnJAzSkKRa2Uy100BR0LZBZgHscdCKIMCol0vyo0t1pSjIVKyup51MJzYYd8Vt9hTwQ1lAvkdc7LQnmzbo4OhT/tQcEVYT+Qh2NDBc0tYH6LwnUle0GKUEgXHcK/somTCO0PDsynPzU0ZgIfHjMEVPikL50Ys1MAxLFUWy42IzaYtPKeEQApmHqkQJveRCibkUHjuuWYMKCVk6xOV0j5zb94hNNnCXyLIZ4UtVBP3raHMRcEtKSUS1Bex4ZXOfFqcrvbOx4SCS4/KtRlxm1U78p1yXkyE5tY8JVEwo+TcMb09Rj9C8UlHvaR9heiNHNGD0UQePKawt8hcT3jvzIFXLjU2Ii+HxJwZl7PRPitpHiVSYCGV4WSNdqjGWi1eIlCjacRHUKvo5luDGp/hV89fqbVLhveB8ffKp3mneIOQzH0OCY68VcMssl6p8RrNeYH+Pzf+xlFlYdR9PuRfTXcs5TfADh+P8Yw2x47rfDwY97aVsPcdgkQR9RnylufgqVLSITLvVOm5Ew71y8Fw3N8w7Ny4FKB0EFu8dygSKXiCXkjjoYcIKz5gBEuRrqjvFJ8zdD0Y/bl0jhSpQ/IFE57nFj63MLoa3aA4dQAhcgZFFNUzVkwVGvS0h8Sz7cg7VPQbxZZLl9C1QOqDJh4hq7EgbcVNW5ShMKaLgY/I8nWuFLcJ6PnJlrz4nRopSkm7DuH5CuauCyRtGBAlnVtQJEN5QUkhQj3AnBFR4oswVQJAybbowSEBtVZtLRDRQkmfm0tweYmCYmHYrhkcNvHBVJmKPtqWgmH9wudQggjvrth2xlXoWj4Pq1X57fq6/nFz0w6AEMwkR3W2jQTEC7JYLMxPbFjwFhU8syg48gCRQku0oA4cHqt3rdxPR0f1+7299bg5PbWjZAxmaqJZkQG/NehSwpzAJI79dQlQXgNxLph5T7x+WwpmIp0TrGs5Z18gmJeLEqaBT79eXj4qtxncOzVI1q2a8BIVS0/QLAPRstEcF0K6v1brDnWfjNqlEAovG+ke99Dns6CBaE7subXo0TIB2b0h8IM2mPVMrTfRsw1ZTHZ2RChR6YGuVA5fdIQOn+u4fDywTQ8OuyBLEOZLgUxn3Lt8OTlZe+6H/f1ae7QLHo83krVspgKhHUt6XONgnwvHqU1o21QwcYBQePKILcbepMWgpbDNft7fVw9VVdjg2l8CDADzOGI6m6udLgAAAABJRU5ErkJggg=="
        let loginLogo = "iVBORw0KGgoAAAANSUhEUgAAAHgAAAAXCAYAAADAxotdAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAABFlJREFUeNrsWr1O3EAQvouuRfIb4Dfg0iNh3uAq2jhPEPIAKE5LY/IEPp7goEwRmTRpKHxRGjqDUNr4kiIFjbN7GaNlbma9s7Z1ys9IqwN7d7ye//nWo9F/+qtpzN2o6zpQP5EaU2bKlRrL8Xi8kj5U8Q6Bt/7dU+NWje/A7+JfELySQQQy2AU5aBncabkqGVwN+mA1FrU7LWCzrrzzFn6VGgkYmNhwYK1+RqGZGfdSuK5Halxv9pPDnKiD7LDcpsScWI2yRQb6fjyEcpPan5IW3qmQXyUwnJAzSkKRa2Uy100BR0LZBZgHscdCKIMCol0vyo0t1pSjIVKyup51MJzYYd8Vt9hTwQ1lAvkdc7LQnmzbo4OhT/tQcEVYT+Qh2NDBc0tYH6LwnUle0GKUEgXHcK/somTCO0PDsynPzU0ZgIfHjMEVPikL50Ys1MAxLFUWy42IzaYtPKeEQApmHqkQJveRCibkUHjuuWYMKCVk6xOV0j5zb94hNNnCXyLIZ4UtVBP3raHMRcEtKSUS1Bex4ZXOfFqcrvbOx4SCS4/KtRlxm1U78p1yXkyE5tY8JVEwo+TcMb09Rj9C8UlHvaR9heiNHNGD0UQePKawt8hcT3jvzIFXLjU2Ii+HxJwZl7PRPitpHiVSYCGV4WSNdqjGWi1eIlCjacRHUKvo5luDGp/hV89fqbVLhveB8ffKp3mneIOQzH0OCY68VcMssl6p8RrNeYH+Pzf+xlFlYdR9PuRfTXcs5TfADh+P8Yw2x47rfDwY97aVsPcdgkQR9RnylufgqVLSITLvVOm5Ew71y8Fw3N8w7Ny4FKB0EFu8dygSKXiCXkjjoYcIKz5gBEuRrqjvFJ8zdD0Y/bl0jhSpQ/IFE57nFj63MLoa3aA4dQAhcgZFFNUzVkwVGvS0h8Sz7cg7VPQbxZZLl9C1QOqDJh4hq7EgbcVNW5ShMKaLgY/I8nWuFLcJ6PnJlrz4nRopSkm7DuH5CuauCyRtGBAlnVtQJEN5QUkhQj3AnBFR4oswVQJAybbowSEBtVZtLRDRQkmfm0tweYmCYmHYrhkcNvHBVJmKPtqWgmH9wudQggjvrth2xlXoWj4Pq1X57fq6/nFz0w6AEMwkR3W2jQTEC7JYLMxPbFjwFhU8syg48gCRQku0oA4cHqt3rdxPR0f1+7299bg5PbWjZAxmaqJZkQG/NehSwpzAJI79dQlQXgNxLph5T7x+WwpmIp0TrGs5Z18gmJeLEqaBT79eXj4qtxncOzVI1q2a8BIVS0/QLAPRstEcF0K6v1brDnWfjNqlEAovG+ke99Dns6CBaE7subXo0TIB2b0h8IM2mPVMrTfRsw1ZTHZ2RChR6YGuVA5fdIQOn+u4fDywTQ8OuyBLEOZLgUxn3Lt8OTlZe+6H/f1ae7QLHo83krVspgKhHUt6XONgnwvHqU1o21QwcYBQePKILcbepMWgpbDNft7fVw9VVdjg2l8CDADzOGI6m6udLgAAAABJRU5ErkJggg=="
        let favicon = "iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAIAAAD8GO2jAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAlhJREFUeNpi1HdNYKAlYGKgMRi1YARYwIJLgpeHy0RPQ11ZzkRfHS747MWbm3cfn7l04+bdRxRZkBEbEB3oCrQDXUIPSgPt6J6+nBhrmCWUDdCEmkqSo4Pc2NlY8WiTEhcJ9XF89vItQTtYMN3u52YDYX/+8m3T7iP7j52Hy6oryUUHuQJNh3BLMyLPXLzx7OUbEnzQVJoMCRmg6SmlnZt2HQXqh6PLN+4uXb8baAEwboBqQL5kZDh25goJqQjuOqDbcXm/rmcu3NXAhEBmKoK4EReYsXgj3Cn4ASNaabptcTdcJzCpQIIIGNBk5wN0C4AxDExFmOqAUXLz3iMQefcxkCQ+KzBi1gelmZHRgW4EdQJ9BowMgp7Dkg+AqeLMpZuQzIwlryFldaB3gSrxJ1NGgjUaMLaBZgFzAKjw0FcHxhBy9AJN94otJScVwQEkrKFBsRialPsbciHJDMg20dfAE1Ao+WBOT/mFXfMhCH/oA7MbOcX1mYs3kZMT3nCTRU5gwNADJo3D66YCEZCB04JNu44g0lJGJC47HK2M/FwR5RUwDDNi/R3Z/pwODAAiV2EOYIGGM5LR0igklwFLTVhBIgwMceRIBhbaS9fvAjocaPSfz1+AIpySkpyNrSklndgjGaiBl5sL7nagWXjCCmg0EIH88fUbKw8vxAIWXh4C+QBYPgNTNzBYFWUlcRkNDMzuGcvXbDkAyxPcbomRH69e5ZFX0Covm7Rq14PHz4nKB8AAAXoIXvABgxvoWKyJEuhRPzdrkLfW7d5/7BwJGW20XTTkLQAIMABRBBtjubVMigAAAABJRU5ErkJggg=="
        runQuery_ . sqlInsert "themes" $  do
          sqlSet "name" ("Scrive mail theme" :: String)
          sqlSet "logo" $ Binary $ B64.decodeLenient $ BS.fromString $ mailLogo
          sqlSet "brand_color" $ ("#ffffff":: String)
          sqlSet "brand_text_color" $ ("#495259":: String)
          sqlSet "action_color" $ ("#53b688":: String)
          sqlSet "action_text_color" $ ("#ffffff":: String)
          sqlSet "action_secondary_color" $ ("#33b1dd":: String)
          sqlSet "action_secondary_text_color" $ ("#ffffff":: String)
          sqlSet "positive_color" $ ("#53b688":: String)
          sqlSet "positive_text_color" $ ("#ffffff":: String)
          sqlSet "negative_color" $ ("#b9322f":: String)
          sqlSet "negative_text_color" $ ("#ffffff":: String)
          sqlSet "font" $ ("Source Sans Pro, Helvetica Neue, Arial, sans-serif" :: String)
          sqlResult "id"
        (mailThemeId::Int64) <- fetchOne unSingle
        runQuery_ . sqlInsert "themes" $  do
          sqlSet "name" ("Scrive main theme" :: String)
          sqlSet "logo" $ Binary $ B64.decodeLenient $ BS.fromString $ mainLogo
          sqlSet "brand_color" $ ("#495259":: String)
          sqlSet "brand_text_color" $ ("#ffffff":: String)
          sqlSet "action_color" $ ("#53b688":: String)
          sqlSet "action_text_color" $ ("#ffffff":: String)
          sqlSet "action_secondary_color" $ ("#33b1dd":: String)
          sqlSet "action_secondary_text_color" $ ("#ffffff":: String)
          sqlSet "positive_color" $ ("#53b688":: String)
          sqlSet "positive_text_color" $ ("#ffffff":: String)
          sqlSet "negative_color" $ ("#b9322f":: String)
          sqlSet "negative_text_color" $ ("#ffffff":: String)
          sqlSet "font" $ ("Source Sans Pro, Helvetica Neue, Arial, sans-serif" :: String)
          sqlResult "id"
        (mainThemeId::Int64) <- fetchOne unSingle
        runQuery_ . sqlInsert "themes" $  do
          sqlSet "name" ("Scrive login theme" :: String)
          sqlSet "logo" $ Binary $ B64.decodeLenient $ BS.fromString $ loginLogo
          sqlSet "brand_color" $ ("#495259":: String)
          sqlSet "brand_text_color" $ ("#ffffff":: String)
          sqlSet "action_color" $ ("#53b688":: String)
          sqlSet "action_text_color" $ ("#ffffff":: String)
          sqlSet "action_secondary_color" $ ("#33b1dd":: String)
          sqlSet "action_secondary_text_color" $ ("#ffffff":: String)
          sqlSet "positive_color" $ ("#53b688":: String)
          sqlSet "positive_text_color" $ ("#ffffff":: String)
          sqlSet "negative_color" $ ("#b9322f":: String)
          sqlSet "negative_text_color" $ ("#ffffff":: String)
          sqlSet "font" $ ("Source Sans Pro, Helvetica Neue, Arial, sans-serif" :: String)
          sqlResult "id"
        (loginThemeId::Int64) <- fetchOne unSingle
        runQuery_ . sqlInsert "branded_domains" $  do
                    sqlSet "url" ("https://scrive.com" :: String)
                    sqlSet "mail_theme" mailThemeId
                    sqlSet "signview_theme" mainThemeId
                    sqlSet "service_theme" mainThemeId
                    sqlSet "login_theme" loginThemeId
                    sqlSet "main_domain" $ True
                    sqlSet "sms_originator" ("Scrive":: String)
                    sqlSet "email_originator" ("Scrive":: String)
                    sqlSet "contact_email" ("noreply@scrive.com":: String)
                    sqlSet "noreply_email" ("noreply@scrive.com":: String)
                    sqlSet "browser_title" ("Scrive":: String)
                    sqlSet "favicon" $ Binary $ B64.decodeLenient $ BS.fromString $ favicon
                    sqlSet "participant_color_1" ("#ff3377":: String)
                    sqlSet "participant_color_2" ("#009999":: String)
                    sqlSet "participant_color_3" ("#ffd700":: String)
                    sqlSet "participant_color_4" ("#7908aa":: String)
                    sqlSet "participant_color_5" ("#53df00":: String)
                    sqlSet "participant_color_6" ("#990000":: String)
                    sqlSet "draft_color" ("#B2B2B2":: String)
                    sqlSet "cancelled_color" ("#D64845":: String)
                    sqlSet "initiated_color" ("#D2793A":: String)
                    sqlSet "sent_color" ("#ECA74D":: String)
                    sqlSet "delivered_color" ("#E7D875":: String)
                    sqlSet "opened_color" ("#54B588":: String)
                    sqlSet "reviewed_color" ("#62C3DE":: String)
                    sqlSet "signed_color" ("#4C4C4C":: String)
                    sqlResult "id"
        (domainId::Int64) <- fetchOne unSingle
        runQuery_ . sqlInsert "theme_owners" $  do
          sqlSet "theme_id" mailThemeId
          sqlSet "domain_id" $ domainId
        runQuery_ . sqlInsert "theme_owners" $  do
          sqlSet "theme_id" mainThemeId
          sqlSet "domain_id" $ domainId
        runQuery_ . sqlInsert "theme_owners" $  do
          sqlSet "theme_id" loginThemeId
          sqlSet "domain_id" $ domainId
    }
  }
