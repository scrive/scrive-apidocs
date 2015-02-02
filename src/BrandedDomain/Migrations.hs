module BrandedDomain.Migrations where

import Control.Monad
import Control.Monad.Catch
import Data.Int
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BS

import BrandedDomain.Tables
import DB
import DB.Checks

createBrandedDomainsTable :: MonadDB m => Migration m
createBrandedDomainsTable =
  Migration {
      mgrTable = tableBrandedDomains
    , mgrFrom = 0
    , mgrDo = do
        createTable $ tblTable {
  tblName = "branded_domains"
  , tblVersion = 1
  , tblColumns =
    [ tblColumn { colName = "id",                            colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "url",                           colType = TextT, colNullable = False }
    , tblColumn { colName = "logolink",                      colType = TextT, colNullable = False }
    , tblColumn { colName = "bars_color",                    colType = TextT, colNullable = False }
    , tblColumn { colName = "bars_text_color",               colType = TextT, colNullable = False }
    , tblColumn { colName = "bars_secondary_color",          colType = TextT, colNullable = False }
    , tblColumn { colName = "background_color",              colType = TextT, colNullable = False }
    , tblColumn { colName = "background_color_external",     colType = TextT, colNullable = False }
    , tblColumn { colName = "mails_background_color",        colType = TextT, colNullable = False }
    , tblColumn { colName = "mails_button_color",            colType = TextT, colNullable = False }
    , tblColumn { colName = "mails_text_color",              colType = TextT, colNullable = False }
    , tblColumn { colName = "signview_primary_color",        colType = TextT, colNullable = False }
    , tblColumn { colName = "signview_primary_text_color",   colType = TextT, colNullable = False }
    , tblColumn { colName = "signview_secondary_color",      colType = TextT, colNullable = False }
    , tblColumn { colName = "signview_secondary_text_color", colType = TextT, colNullable = False }
    , tblColumn { colName = "button_class",                  colType = TextT, colNullable = False }
    , tblColumn { colName = "service_link_color",            colType = TextT, colNullable = False }
    , tblColumn { colName = "external_text_color",           colType = TextT, colNullable = False }
    , tblColumn { colName = "header_color",                  colType = TextT, colNullable = False }
    , tblColumn { colName = "text_color",                    colType = TextT, colNullable = False }
    , tblColumn { colName = "price_color",                   colType = TextT, colNullable = False }
    , tblColumn { colName = "sms_originator",                colType = TextT, colNullable = False }
    , tblColumn { colName = "email_originator",              colType = TextT, colNullable = False }
    , tblColumn { colName = "contact_email",                 colType = TextT, colNullable = False }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  , tblIndexes = [
      indexOnColumn "url"
    ]
  }

}


addLogoImageDataToBrandedDomain :: MonadDB m => Migration m
addLogoImageDataToBrandedDomain =
  Migration {
      mgrTable = tableBrandedDomains
    , mgrFrom = 1
    , mgrDo = do
        runSQL_ "ALTER TABLE branded_domains DROP COLUMN logolink"
        runSQL_ "ALTER TABLE branded_domains ADD COLUMN logo BYTEA NULL"
    }

addNoReplyEmailToBrandedDomain :: MonadDB m => Migration m
addNoReplyEmailToBrandedDomain =
  Migration {
      mgrTable = tableBrandedDomains
    , mgrFrom = 2
    , mgrDo = do
        runSQL_ "ALTER TABLE branded_domains ADD COLUMN noreply_email TEXT NOT NULL DEFAULT ''"
    }

addNoReplyEmailToBrandedDomainSetDefault :: MonadDB m => Migration m
addNoReplyEmailToBrandedDomainSetDefault =
  Migration {
      mgrTable = tableBrandedDomains
    , mgrFrom = 3
    , mgrDo = do
        runSQL_ "ALTER TABLE branded_domains ALTER COLUMN noreply_email SET DEFAULT ''"
    }

addMailsBorderColorToBrandedDomain :: MonadDB m => Migration m
addMailsBorderColorToBrandedDomain =
  Migration {
      mgrTable = tableBrandedDomains
    , mgrFrom = 4
    , mgrDo = do
        runSQL_ "ALTER TABLE branded_domains ADD COLUMN mails_border_color TEXT NOT NULL DEFAULT ''"
    }

addThemesToBrandedDomainAndMainDomain ::(MonadDB m, MonadThrow m) => Migration m
addThemesToBrandedDomainAndMainDomain =
  Migration {
      mgrTable = tableBrandedDomains
    , mgrFrom = 5
    , mgrDo = do
        -- Setting up table
        runSQL_ "ALTER TABLE branded_domains ADD COLUMN mail_theme BIGINT NULL"
        runSQL_ "ALTER TABLE branded_domains ADD COLUMN signview_theme BIGINT NULL"
        runSQL_ "ALTER TABLE branded_domains ADD COLUMN service_theme BIGINT NULL"
        runSQL_ "ALTER TABLE branded_domains ADD COLUMN login_theme BIGINT NULL"
        runSQL_ "ALTER TABLE branded_domains ADD COLUMN main_domain BOOL NOT NULL DEFAULT FALSE"

        runSQL_ "ALTER TABLE branded_domains ADD COLUMN browser_title TEXT NULL"
        runSQL_ "ALTER TABLE branded_domains ADD COLUMN favicon BYTEA NULL"

        runSQL_ "ALTER TABLE branded_domains ADD COLUMN participant_color_1 color NULL"
        runSQL_ "ALTER TABLE branded_domains ADD COLUMN participant_color_2 color NULL"
        runSQL_ "ALTER TABLE branded_domains ADD COLUMN participant_color_3 color NULL"
        runSQL_ "ALTER TABLE branded_domains ADD COLUMN participant_color_4 color NULL"
        runSQL_ "ALTER TABLE branded_domains ADD COLUMN participant_color_5 color NULL"
        runSQL_ "ALTER TABLE branded_domains ADD COLUMN participant_color_6 color NULL"
        runSQL_ "ALTER TABLE branded_domains ADD COLUMN draft_color color NULL"
        runSQL_ "ALTER TABLE branded_domains ADD COLUMN cancelled_color color NULL"
        runSQL_ "ALTER TABLE branded_domains ADD COLUMN initiated_color color NULL"
        runSQL_ "ALTER TABLE branded_domains ADD COLUMN sent_color color NULL"
        runSQL_ "ALTER TABLE branded_domains ADD COLUMN delivered_color color NULL"
        runSQL_ "ALTER TABLE branded_domains ADD COLUMN opened_color color NULL"
        runSQL_ "ALTER TABLE branded_domains ADD COLUMN reviewed_color color NULL"
        runSQL_ "ALTER TABLE branded_domains ADD COLUMN signed_color color NULL"


        -- Creating themes and connecting them
        runQuery_ $ sqlSelect "branded_domains" $ do
                 sqlResult "id"
                 sqlResult "url"
                 sqlResult "logo"
        domains :: [(Int64, String, Binary BS.ByteString)] <- fetchMany id
        forM_ domains $ \(did, durl, dlogo) -> do
            runQuery_ . sqlInsert "themes" $  do
              sqlSet "name" (durl ++ "_theme")
              sqlSet "logo" (dlogo)
              sqlSet "brand_color" $ ("#000000":: String)
              sqlSet "brand_text_color" $ ("#FFFFFF":: String)
              sqlSet "action_color" $ ("#000000":: String)
              sqlSet "action_text_color" $ ("#FFFFFF":: String)
              sqlSet "action_secondary_color" $ ("#000000":: String)
              sqlSet "action_secondary_text_color" $ ("#FFFFFF":: String)
              sqlSet "positive_color" $ ("#000000":: String)
              sqlSet "positive_text_color" $ ("#FFFFFF":: String)
              sqlSet "negative_color" $ ("#000000":: String)
              sqlSet "negative_text_color" $ ("#FFFFFF":: String)
              sqlSet "font" $ ("\"Source Sans Pro\", \"Helvetica Neue\", sans-serif" :: String)
              sqlResult "id"
            (themeId::Int64) <- fetchOne unSingle
            runQuery_ . sqlInsert "theme_owners" $  do
              sqlSet "theme_id" themeId
              sqlSet "domain_id" $ did
            runQuery_ $ sqlUpdate "branded_domains" $ do
              sqlSet "mail_theme" themeId
              sqlSet "signview_theme" themeId
              sqlSet "service_theme" themeId
              sqlSet "login_theme" themeId
              sqlSet "browser_title" ("Scrive":: String)
              sqlSet "favicon" $ dlogo
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
              sqlWhereEq "id" did

        -- Adding CONSTRAINTs
        runSQL_ "ALTER TABLE branded_domains ALTER mail_theme SET NOT NULL"
        runSQL_ "ALTER TABLE branded_domains ALTER signview_theme SET NOT NULL"
        runSQL_ "ALTER TABLE branded_domains ALTER service_theme SET NOT NULL"
        runSQL_ "ALTER TABLE branded_domains ALTER login_theme SET NOT NULL"
        runQuery_ $ sqlAlterTable "branded_domains" [sqlAddFK "branded_domains" $  (fkOnColumn "mail_theme" "themes" "id") {fkOnDelete = ForeignKeyCascade , fkDeferred = True}]
        runQuery_ $ sqlAlterTable "branded_domains" [sqlAddFK "branded_domains" $  (fkOnColumn "signview_theme" "themes" "id") {fkOnDelete = ForeignKeyCascade, fkDeferred = True }]
        runQuery_ $ sqlAlterTable "branded_domains" [sqlAddFK "branded_domains" $  (fkOnColumn "service_theme" "themes" "id") {fkOnDelete = ForeignKeyCascade, fkDeferred = True }]
        runQuery_ $ sqlAlterTable "branded_domains" [sqlAddFK "branded_domains" $  (fkOnColumn "login_theme" "themes" "id") {fkOnDelete = ForeignKeyCascade, fkDeferred = True }]
        runQuery_ $ sqlCreateIndex "branded_domains" $ uniqueIndexOnColumnWithCondition "main_domain" "main_domain"


        runSQL_ "ALTER TABLE branded_domains ALTER browser_title SET NOT NULL"
        runSQL_ "ALTER TABLE branded_domains ALTER favicon SET NOT NULL"
        runSQL_ "ALTER TABLE branded_domains ALTER participant_color_1  SET NOT NULL"
        runSQL_ "ALTER TABLE branded_domains ALTER participant_color_2  SET NOT NULL"
        runSQL_ "ALTER TABLE branded_domains ALTER participant_color_3  SET NOT NULL"
        runSQL_ "ALTER TABLE branded_domains ALTER participant_color_4  SET NOT NULL"
        runSQL_ "ALTER TABLE branded_domains ALTER participant_color_5  SET NOT NULL"
        runSQL_ "ALTER TABLE branded_domains ALTER participant_color_6  SET NOT NULL"
        runSQL_ "ALTER TABLE branded_domains ALTER draft_color  SET NOT NULL"
        runSQL_ "ALTER TABLE branded_domains ALTER cancelled_color  SET NOT NULL"
        runSQL_ "ALTER TABLE branded_domains ALTER initiated_color  SET NOT NULL"
        runSQL_ "ALTER TABLE branded_domains ALTER sent_color  SET NOT NULL"
        runSQL_ "ALTER TABLE branded_domains ALTER delivered_color  SET NOT NULL"
        runSQL_ "ALTER TABLE branded_domains ALTER opened_color  SET NOT NULL"
        runSQL_ "ALTER TABLE branded_domains ALTER reviewed_color  SET NOT NULL"
        runSQL_ "ALTER TABLE branded_domains ALTER signed_color  SET NOT NULL"

        runSQL_ "ALTER TABLE branded_domains DROP COLUMN bars_color"
        runSQL_ "ALTER TABLE branded_domains DROP COLUMN bars_text_color"
        runSQL_ "ALTER TABLE branded_domains DROP COLUMN bars_secondary_color"
        runSQL_ "ALTER TABLE branded_domains DROP COLUMN background_color"
        runSQL_ "ALTER TABLE branded_domains DROP COLUMN background_color_external"
        runSQL_ "ALTER TABLE branded_domains DROP COLUMN mails_background_color"
        runSQL_ "ALTER TABLE branded_domains DROP COLUMN mails_button_color"
        runSQL_ "ALTER TABLE branded_domains DROP COLUMN mails_text_color"
        runSQL_ "ALTER TABLE branded_domains DROP COLUMN signview_primary_color"
        runSQL_ "ALTER TABLE branded_domains DROP COLUMN signview_primary_text_color"
        runSQL_ "ALTER TABLE branded_domains DROP COLUMN signview_secondary_color"
        runSQL_ "ALTER TABLE branded_domains DROP COLUMN signview_secondary_text_color"
        runSQL_ "ALTER TABLE branded_domains DROP COLUMN button_class"
        runSQL_ "ALTER TABLE branded_domains DROP COLUMN service_link_color"
        runSQL_ "ALTER TABLE branded_domains DROP COLUMN external_text_color"
        runSQL_ "ALTER TABLE branded_domains DROP COLUMN header_color"
        runSQL_ "ALTER TABLE branded_domains DROP COLUMN text_color"
        runSQL_ "ALTER TABLE branded_domains DROP COLUMN price_color"
        runSQL_ "ALTER TABLE branded_domains DROP COLUMN logo"
        runSQL_ "ALTER TABLE branded_domains DROP COLUMN mails_border_color"

        addMainDomain
    }

addMainDomain :: (MonadDB m, MonadThrow m) => m ()
addMainDomain = do
        let mailLogo = "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"
        let mainLogo = "iVBORw0KGgoAAAANSUhEUgAAAHgAAAAXCAYAAADAxotdAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAABFlJREFUeNrsWr1O3EAQvouuRfIb4Dfg0iNh3uAq2jhPEPIAKE5LY/IEPp7goEwRmTRpKHxRGjqDUNr4kiIFjbN7GaNlbma9s7Z1ys9IqwN7d7ye//nWo9F/+qtpzN2o6zpQP5EaU2bKlRrL8Xi8kj5U8Q6Bt/7dU+NWje/A7+JfELySQQQy2AU5aBncabkqGVwN+mA1FrU7LWCzrrzzFn6VGgkYmNhwYK1+RqGZGfdSuK5Halxv9pPDnKiD7LDcpsScWI2yRQb6fjyEcpPan5IW3qmQXyUwnJAzSkKRa2Uy100BR0LZBZgHscdCKIMCol0vyo0t1pSjIVKyup51MJzYYd8Vt9hTwQ1lAvkdc7LQnmzbo4OhT/tQcEVYT+Qh2NDBc0tYH6LwnUle0GKUEgXHcK/somTCO0PDsynPzU0ZgIfHjMEVPikL50Ys1MAxLFUWy42IzaYtPKeEQApmHqkQJveRCibkUHjuuWYMKCVk6xOV0j5zb94hNNnCXyLIZ4UtVBP3raHMRcEtKSUS1Bex4ZXOfFqcrvbOx4SCS4/KtRlxm1U78p1yXkyE5tY8JVEwo+TcMb09Rj9C8UlHvaR9heiNHNGD0UQePKawt8hcT3jvzIFXLjU2Ii+HxJwZl7PRPitpHiVSYCGV4WSNdqjGWi1eIlCjacRHUKvo5luDGp/hV89fqbVLhveB8ffKp3mneIOQzH0OCY68VcMssl6p8RrNeYH+Pzf+xlFlYdR9PuRfTXcs5TfADh+P8Yw2x47rfDwY97aVsPcdgkQR9RnylufgqVLSITLvVOm5Ew71y8Fw3N8w7Ny4FKB0EFu8dygSKXiCXkjjoYcIKz5gBEuRrqjvFJ8zdD0Y/bl0jhSpQ/IFE57nFj63MLoa3aA4dQAhcgZFFNUzVkwVGvS0h8Sz7cg7VPQbxZZLl9C1QOqDJh4hq7EgbcVNW5ShMKaLgY/I8nWuFLcJ6PnJlrz4nRopSkm7DuH5CuauCyRtGBAlnVtQJEN5QUkhQj3AnBFR4oswVQJAybbowSEBtVZtLRDRQkmfm0tweYmCYmHYrhkcNvHBVJmKPtqWgmH9wudQggjvrth2xlXoWj4Pq1X57fq6/nFz0w6AEMwkR3W2jQTEC7JYLMxPbFjwFhU8syg48gCRQku0oA4cHqt3rdxPR0f1+7299bg5PbWjZAxmaqJZkQG/NehSwpzAJI79dQlQXgNxLph5T7x+WwpmIp0TrGs5Z18gmJeLEqaBT79eXj4qtxncOzVI1q2a8BIVS0/QLAPRstEcF0K6v1brDnWfjNqlEAovG+ke99Dns6CBaE7subXo0TIB2b0h8IM2mPVMrTfRsw1ZTHZ2RChR6YGuVA5fdIQOn+u4fDywTQ8OuyBLEOZLgUxn3Lt8OTlZe+6H/f1ae7QLHo83krVspgKhHUt6XONgnwvHqU1o21QwcYBQePKILcbepMWgpbDNft7fVw9VVdjg2l8CDADzOGI6m6udLgAAAABJRU5ErkJggg=="
        let loginLogo = "iVBORw0KGgoAAAANSUhEUgAAAHgAAAAXCAYAAADAxotdAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAABFlJREFUeNrsWr1O3EAQvouuRfIb4Dfg0iNh3uAq2jhPEPIAKE5LY/IEPp7goEwRmTRpKHxRGjqDUNr4kiIFjbN7GaNlbma9s7Z1ys9IqwN7d7ye//nWo9F/+qtpzN2o6zpQP5EaU2bKlRrL8Xi8kj5U8Q6Bt/7dU+NWje/A7+JfELySQQQy2AU5aBncabkqGVwN+mA1FrU7LWCzrrzzFn6VGgkYmNhwYK1+RqGZGfdSuK5Halxv9pPDnKiD7LDcpsScWI2yRQb6fjyEcpPan5IW3qmQXyUwnJAzSkKRa2Uy100BR0LZBZgHscdCKIMCol0vyo0t1pSjIVKyup51MJzYYd8Vt9hTwQ1lAvkdc7LQnmzbo4OhT/tQcEVYT+Qh2NDBc0tYH6LwnUle0GKUEgXHcK/somTCO0PDsynPzU0ZgIfHjMEVPikL50Ys1MAxLFUWy42IzaYtPKeEQApmHqkQJveRCibkUHjuuWYMKCVk6xOV0j5zb94hNNnCXyLIZ4UtVBP3raHMRcEtKSUS1Bex4ZXOfFqcrvbOx4SCS4/KtRlxm1U78p1yXkyE5tY8JVEwo+TcMb09Rj9C8UlHvaR9heiNHNGD0UQePKawt8hcT3jvzIFXLjU2Ii+HxJwZl7PRPitpHiVSYCGV4WSNdqjGWi1eIlCjacRHUKvo5luDGp/hV89fqbVLhveB8ffKp3mneIOQzH0OCY68VcMssl6p8RrNeYH+Pzf+xlFlYdR9PuRfTXcs5TfADh+P8Yw2x47rfDwY97aVsPcdgkQR9RnylufgqVLSITLvVOm5Ew71y8Fw3N8w7Ny4FKB0EFu8dygSKXiCXkjjoYcIKz5gBEuRrqjvFJ8zdD0Y/bl0jhSpQ/IFE57nFj63MLoa3aA4dQAhcgZFFNUzVkwVGvS0h8Sz7cg7VPQbxZZLl9C1QOqDJh4hq7EgbcVNW5ShMKaLgY/I8nWuFLcJ6PnJlrz4nRopSkm7DuH5CuauCyRtGBAlnVtQJEN5QUkhQj3AnBFR4oswVQJAybbowSEBtVZtLRDRQkmfm0tweYmCYmHYrhkcNvHBVJmKPtqWgmH9wudQggjvrth2xlXoWj4Pq1X57fq6/nFz0w6AEMwkR3W2jQTEC7JYLMxPbFjwFhU8syg48gCRQku0oA4cHqt3rdxPR0f1+7299bg5PbWjZAxmaqJZkQG/NehSwpzAJI79dQlQXgNxLph5T7x+WwpmIp0TrGs5Z18gmJeLEqaBT79eXj4qtxncOzVI1q2a8BIVS0/QLAPRstEcF0K6v1brDnWfjNqlEAovG+ke99Dns6CBaE7subXo0TIB2b0h8IM2mPVMrTfRsw1ZTHZ2RChR6YGuVA5fdIQOn+u4fDywTQ8OuyBLEOZLgUxn3Lt8OTlZe+6H/f1ae7QLHo83krVspgKhHUt6XONgnwvHqU1o21QwcYBQePKILcbepMWgpbDNft7fVw9VVdjg2l8CDADzOGI6m6udLgAAAABJRU5ErkJggg=="
        let favicon = "iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAA4RpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDUuNi1jMDE0IDc5LjE1Njc5NywgMjAxNC8wOC8yMC0wOTo1MzowMiAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wTU09Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9tbS8iIHhtbG5zOnN0UmVmPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvc1R5cGUvUmVzb3VyY2VSZWYjIiB4bWxuczp4bXA9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC8iIHhtcE1NOk9yaWdpbmFsRG9jdW1lbnRJRD0ieG1wLmRpZDpiMWYzNzg3MS00YzVlLTNmNDEtOWJmYS02YzE2ZTAxYTZlOTAiIHhtcE1NOkRvY3VtZW50SUQ9InhtcC5kaWQ6QTNEQ0Y1MjBBMzBGMTFFNEI1MUI4QjU5NzQ3QjEwRkUiIHhtcE1NOkluc3RhbmNlSUQ9InhtcC5paWQ6QTNEQ0Y1MUZBMzBGMTFFNEI1MUI4QjU5NzQ3QjEwRkUiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENDIDIwMTQgKFdpbmRvd3MpIj4gPHhtcE1NOkRlcml2ZWRGcm9tIHN0UmVmOmluc3RhbmNlSUQ9InhtcC5paWQ6YjFmMzc4NzEtNGM1ZS0zZjQxLTliZmEtNmMxNmUwMWE2ZTkwIiBzdFJlZjpkb2N1bWVudElEPSJhZG9iZTpkb2NpZDpwaG90b3Nob3A6MDFjNGUwNDItNWY0My0xMWU0LWE1ZGItZjNlMTY0YjQ0ZGY1Ii8+IDwvcmRmOkRlc2NyaXB0aW9uPiA8L3JkZjpSREY+IDwveDp4bXBtZXRhPiA8P3hwYWNrZXQgZW5kPSJyIj8+Pn7U7QAABBZJREFUeNrEV2tMk1cYflpAsJQCLYUWEOQy7YZxIuUi02hABU1G2ObMNIvLNHMEjE5Fjf7wFhNmvCSLxEs0mfuBmXFsauImm27Lpq5UxYg6mdMBcimUXigtBOTmOceP6mdb2hJYn+TNl3PO9533Oed93+d8R7CocDk45BErJZZJLAQTgy5iWmIHiVXRDiE3sIvYZWILJ9A5hYTzQX3tpB3+3Mp34//HHmLVdAc2w3copQQyfEgggxII9SEBib8nb72RlIgFc+cgRaVCTLQCoRIJ6+/v70d3Tw8anjah7t/H+OO6Bo//q/eKgYCU4bCrQXmEDJtKipA2a6bHE96pvY8jJ06huVXn0ftCVwNRkXJ89eU+B+cDAwMwGE142twCU2cnhof5/FNnzkD5gTIkJyZ4RMBlCDYUfYYImdTerrlbi4pz3+Nh3SMMDA7a+0PEYkZyxbL3kBAfx/pEosnYsWk9Pt+4lYXJ6x2IksuhTn2b53zH3jLce/CQ55zCarPh92s3UFK6nb03gtiYaLyTmT62EEyNn8JrX9fcxNDQ0KgT0dCUn/ya1+dJ7jgNwevO4uNiPYpnc0srdpcdIKktYG2LpWtsBJ7UN7LkEnATvZu/GJMCAthOPKj7B7bubpcT3tDeGp8y/KJ4LZYuynX6UafFQlarQ5teD73BAL3egPaODjQ2NbMK8QYuq+DYqdOQhoUhKz3NYSwsNJTZjLdUTsPw3cVL+OmXqw4l6gx+SaoUpyfhIMl2mt1P6hsgDg6GTBoOf3/3wimRhDDSSQlTcU2jdZu8oyohr1yEQshlMigUkVBERkJJnsqoKGax0UqIxcEO35whunH6zNnxIeAOMUolli7OwYeFBfY+m60bH6xaM2oonO5p0aerIBC+qIDGphb8+PMVtwRadDqc/KaCaEgc0lNnsT66K+EkV6hke0VgTqaabS1Fh8GIqqu/sZzwBFarjdfue/bMeyXU3r7DOxFXf7zCI+cRJEcy1bPt7VZdGzuu6WIKluQhm0gzzSW3VaBrayfxzIWfnx9rp6im483p09BltcJkNhPZHXQ4tnPnz8O2Devs/woUFecq0dvXh6OH9yM7Q03+KbIhDQ+D5laN+yTMX5iDjUSMRtRwBDShqNhQNZwcFMT0ICgo0OF7zc3b2LP/ED5ZuRwfvV/IK+8ly1a6F6LLV35FW7se69auRlxszEvGhBBdMTVn6O3tw9kfLuDbyvPMmclk5o2bOy3elSF1SH8ystLVmJacSDRAjmCRCIGBL1ZNY2wkTuobGlFTew9//lXNym8EAeQM2bK+GHOzMtBhNOJw+XHcvf/3+OvAWCHkrku+QhclUO1DAlpK4JAPCRwUcrdUX9wNqc8q4SsXxXxiVPStE+jUyvnI53ziuQADAOgNb6y3aUMlAAAAAElFTkSuQmCC"

        runQuery_ . sqlInsert "themes" $  do
          sqlSet "name" ("Scrive email theme" :: String)
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
          sqlSet "font" $ ("\"Source Sans Pro\", \"Helvetica Neue\", Arial, sans-serif" :: String)
          sqlResult "id"
        (mailThemeId::Int64) <- fetchOne unSingle
        runQuery_ . sqlInsert "themes" $  do
          sqlSet "name" ("Scrive service theme" :: String)
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
          sqlSet "font" $ ("\"Source Sans Pro\", \"Helvetica Neue\", Arial, sans-serif" :: String)
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
          sqlSet "font" $ ("\"Source Sans Pro\", \"Helvetica Neue\", Arial, sans-serif" :: String)
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
                    sqlSet "draft_color" ("#b2b2b2":: String)
                    sqlSet "cancelled_color" ("#d64845":: String)
                    sqlSet "initiated_color" ("#d2793a":: String)
                    sqlSet "sent_color" ("#eca74d":: String)
                    sqlSet "delivered_color" ("#e7d875":: String)
                    sqlSet "opened_color" ("#54b588":: String)
                    sqlSet "reviewed_color" ("#62c3de":: String)
                    sqlSet "signed_color" ("#4c4c4c":: String)
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
