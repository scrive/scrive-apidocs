module BrandedDomain.Tables (tableBrandedDomains) where

import Data.Int
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.UTF8 as BS

import DB

tableBrandedDomains :: Table
tableBrandedDomains = tblTable
  { tblName         = "branded_domains"
  , tblVersion      = 12
  , tblColumns      =
    [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "url", colType = TextT, colNullable = False }
    , tblColumn { colName = "sms_originator", colType = TextT, colNullable = False }
    , tblColumn { colName = "email_originator", colType = TextT, colNullable = False }
    , tblColumn { colName = "mail_theme", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "signview_theme", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "service_theme", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "login_theme", colType = BigIntT, colNullable = False }
    , tblColumn { colName     = "main_domain"
                , colType     = BoolT
                , colNullable = False
                , colDefault  = Just "false"
                }
    , tblColumn { colName = "browser_title", colType = TextT, colNullable = False }
    , tblColumn { colName = "favicon", colType = BinaryT, colNullable = False }
    , tblColumn { colName     = "participant_color_1"
                , colType     = CustomT "color"
                , colNullable = False
                }
    , tblColumn { colName     = "participant_color_2"
                , colType     = CustomT "color"
                , colNullable = False
                }
    , tblColumn { colName     = "participant_color_3"
                , colType     = CustomT "color"
                , colNullable = False
                }
    , tblColumn { colName     = "participant_color_4"
                , colType     = CustomT "color"
                , colNullable = False
                }
    , tblColumn { colName     = "participant_color_5"
                , colType     = CustomT "color"
                , colNullable = False
                }
    , tblColumn { colName     = "participant_color_6"
                , colType     = CustomT "color"
                , colNullable = False
                }
    , tblColumn { colName     = "draft_color"
                , colType     = CustomT "color"
                , colNullable = False
                }
    , tblColumn { colName     = "cancelled_color"
                , colType     = CustomT "color"
                , colNullable = False
                }
    , tblColumn { colName     = "initiated_color"
                , colType     = CustomT "color"
                , colNullable = False
                }
    , tblColumn { colName = "sent_color", colType = CustomT "color", colNullable = False }
    , tblColumn { colName     = "delivered_color"
                , colType     = CustomT "color"
                , colNullable = False
                }
    , tblColumn { colName     = "opened_color"
                , colType     = CustomT "color"
                , colNullable = False
                }
    , tblColumn { colName     = "reviewed_color"
                , colType     = CustomT "color"
                , colNullable = False
                }
    , tblColumn { colName     = "signed_color"
                , colType     = CustomT "color"
                , colNullable = False
                }
    ]
  , tblPrimaryKey   = pkOnColumn "id"
  , tblIndexes      = [ indexOnColumn "url"
                      , uniqueIndexOnColumnWithCondition "main_domain" "main_domain" -- Constrain to have only one main domain
                      ]
  , tblForeignKeys  =
    [ (fkOnColumn "mail_theme" "themes" "id") { fkOnDelete = ForeignKeyCascade
                                              , fkDeferred = True
                                              }
    , (fkOnColumn "signview_theme" "themes" "id") { fkOnDelete = ForeignKeyCascade
                                                  , fkDeferred = True
                                                  }
    , (fkOnColumn "service_theme" "themes" "id") { fkOnDelete = ForeignKeyCascade
                                                 , fkDeferred = True
                                                 }
    , (fkOnColumn "login_theme" "themes" "id") { fkOnDelete = ForeignKeyCascade
                                               , fkDeferred = True
                                               }
    ]
  , tblInitialSetup =
    Just $ TableInitialSetup
      { checkInitialSetup = do
                              runQuery_ . sqlSelect "branded_domains" $ do
                                sqlWhere "main_domain"
                                sqlResult "id"
                              (mainDomains :: [Int64]) <- fetchMany runIdentity
                              return $ length mainDomains == 1
      , initialSetup      =
        do
        -- Currently both : Base64 encoded PNG, 240x46 pixels
          let
            darkTextLogo
              = "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"
          let
            lightTextLogo
              = "iVBORw0KGgoAAAANSUhEUgAAAPAAAAAuCAMAAAAhv2T/AAAAq1BMVEUAAAD////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////Nr6iZAAAAOHRSTlMA7/wK+usO99IkBvO/eBrWAo22UxIE4saTOhWrmYN9YjQpzbGlMOaHTtpzoEk+X7otHt5pZlpDbQqMiCkAAAcBSURBVGjevVpng6IwEJUqIHZFLCj23k7Xzf//ZVcMvABJwNO792k3Ccm8TMlMYimBWns9nHmKriorqzX6PmolObTDel43yrpSbwUPp5SPzqbZqiv6n78JxZ9/fk+x3pi5E1zoR1Z64naztVX0ymq2qIaNUiFMA4ukUHa/a8Lxjc28zA6u7Ps52zNxVXAEYfyjn79y9nhNP9qxjc6JTkxh+NN8uscz4UIfXvh0T/XsYFdCuRMYdBSfMMWqKqX84zlKZfTQvXkkg+FSTnewIEKoI46lhTP+4H1HYA430OUSBry2RC10TAtNfYuvqKZs45YekeHHNC1+VReODXkLXF2MEBEGhh2RoCM64iuWZKwSAeoXId+wQuRQkgaitSRjy22OYgxSgDCwFcjq0HkqWhFJKhsB3ysrjuHu18F4PWr9SISBK+uNPSKDSrcfOOmkEGFAOXIl3USOQ/835ZLoX3zCLqxgZzN+PUEH6TWg3x5DrjfeTK92v920mMZHcv4Jq//5d3/gZAkvjxO/zmqHG/6GtJcu4LjMou7uaGuafdy5KhqhY548yind1UdomsRe04IeAkbxy70at9vsJAcI4O3gnSDMTKFjbs7JYpbpNA3q0dDliFnSHsXTVDizOB4cNdsZz1mPmsYkwqiTCn2x6mddZnkl1u4YgTNBGLgO4cfZs+FEu9bPf7/isVbK5y9W3JON1W3okIf4vKLbEUb6UrLm0l0TiiraZvGW0Rl4hIEvI47VmfmjVOGptkG8k76TUZQf9TWFjCoOP18wEhwakfyrC9c9VKrMODGoxklJpyQhDNjbqCMd7mt08hkMGpIByXX1jFFH889ykrlhwuEN8OVa3SgSshLx1UpywuAVSeSlPtnR9u/nxqjQIQ9NkaGUozgsINxn0/UuFUY9YAB3nfLg+a8f2bNZyiEM2AZfdTOqs0FCwW5DkNu5VFQ71VGJdCb40Ix8FgchdjULp84KW9Mp/0splzBwiERKqHhKW+eJiK0Iq5uawpfVhcvkY04dGJKIpLUSnjQuvUAYceXGNgYJOU+wbxHGVNiG6Bg+5PI1dRRnhWBRb3ReIQzP73EKJUUruvXa6jnmmD2HKc4n2IcstSt3CvK9Qg2vEIY27WQsQThsUHsNZMsHfPO6JYqd4bh9Ee7aGl5UCBNs0GuEa2pGVp9AX3BocaWPQWd+mGGx6i3W98NUE7jwrShhHxv0CmFEli0SGINqBOlSUXgFqkMQ3/UZn6db0y9KuIcNeokwIks/HbkD5BWFkVHcoSwbvmrGJQJ1nEFRwh6kfpGwSZXgpwO3DecqjGxkCn/I60pfS8joFCVcxgYVIwzs6VFM19IQt+EsRWFzIvhYITLUp39FGCb1ImHcXm2SldHtTcKAeT/rRAzj8jcmDUqvEMaxi1S4hXj/iknL5XXCSbDoGYLLuY40aH2ecEApmn9KQR30UUjkophFmsv2bnTepvU9wrH0/T8I22yZ/g0Df2KDePQpdO3HbrEFYfWKxOPfE8ZR7DLnm+Gkt+Or9GFc9oSgADjAk/494VO00aDno3cFG/8scHN0RvFQ/feEcRJVcY8WoneB4vizwN3oFk5smOLhD5SH7xBGiV+Pg2WdcxUXyApViAIotNXMVbGCZYQXALiXqL5LGPXRMuRU1RoVvFKTXEXwilmLth5yH7BWvyOZl3fFc8MVz3uEcd3WbMKbgSbe1QDegEonk8LJvf/EWsYt5xIvRA78PuEq3ekV4jVQ03HzJRObrPnVsL6U1Hh40OlGFuFxK9GlAUN7izCqYmDC1yA58b69R98apmDSmaDsH0Sp1506FoXBseoH7hvfJ4yEEjkXC9OjHeq3+EkAV3Xa1G7Qop7izI1bmkuSB0BAIvhmamgQb173TcKIl8CCcx5QDAf8Rx88NJp+hRCj+ptyiHR5w/FJK71e44yKotphqw+PeUx7nzBCsTCsYu+VMSPJdA9X6GlP4SyGxB5zWjs7kc7eXRgU7cHHeP3UauFkUUb5/Hgz8QBGWGrV5V3nQpLhLRw4nelmPSNAHe8B8HdzSxh4LX98u9+rwd5V2PY749UzIoPe/kimhYghPvqdFpHCqtFxFcK+r9geEUCwnOnKf2jwNmEAF4zcU8RZEAlancjIYQjPSD0jcqjVVBAMVNHQLQT7BOFqrCzBgJ0utLRxA7VVBIUSGJelltHP2pol/InTRwnX9GR+WPwHVD1m5xsrks7Lrn5FSHfS5ZXL9x8cGwrfqJakR7Eq+UUgjgdg1ua+tJIH2szJnMN56x+FNwTtecKclBHofo5wG68HQnS/zmpCkn1a6MaQm4Z2w3tz3vOUX8QVo+4ugvZVLo25aZ7rhq4b1nz8gB18krBj8NNH7o9L1bJhDatHnkVOehWl9Uu/PwFHFY/VJCptfQAAAABJRU5ErkJggg=="
          -- Currently: Base64 encoded ICO file, 32x32 pixels
          let
            favicon
              = "iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAMAAABEpIrGAAAABGdBTUEAALGPC/xhBQAAAAFzUkdCAK7OHOkAAAAJcEhZcwAACxMAAAsTAQCanBgAAABCUExURVBQUFJSUktLS/r6+lBQUP///0VFRVBQUFBQUE5OTmVlZVpaWvDw8MLCwrKysubm5t3d3Y+Pj4ODg3d3d6WlpdLS0jBFWHAAAAAJdFJOU5n///////8ImNkf7DAAAAFcSURBVDjLhVNbloMgDCUR0kbeAvvf6gTUam3H5sN66DW5j6AeT31Tz4dS+raUgnsA/GggLQ4sAPeS368AQEKw1moigk+A/F1qi8652GoR6AUAaL05qs1HkxWANprJGJdackbe3PzqMQBAXk59AUa2S5P3pPkMwFk+90T9kGmgl32IGg2WcWQ3Piv8A1AJuZshVZalfI6YQhkmEGF/6HeSwUwdE1uoucxMcJHJfLZhShn54iTT0tx0YDxenAQgsnPJ1bfYZwnlk4quXeznQQ5hrtIiAh8AhhpCFnNhRI3ilJnEbDhGRGMS6q2pFU2SxiZV7Spl6mo1kpXAEs116VpeRvWwLKNQyD3ZbIVrEKabUdnsccch1lPtqVveZVJJJ6dcZfmkh36sHHEJ68oln63Q4zDFgvBaWtC0Lq1sJzF0b4TRZe31WHu9BSVOwRvgv4vz8+r9aPFUD3V//f8ANZsWxjENgZ8AAAAASUVORK5CYII="
          runQuery_ . sqlInsert "themes" $ do
            sqlSet "name" ("Scrive email theme" :: String)
            sqlSet "logo" $ B64.decodeLenient $ BS.fromString $ darkTextLogo
            sqlSet "brand_color" $ ("#ffffff" :: String)
            sqlSet "brand_text_color" $ ("#495259" :: String)
            sqlSet "action_color" $ ("#53b688" :: String)
            sqlSet "action_text_color" $ ("#ffffff" :: String)
            sqlSet "action_secondary_color" $ ("#33b1dd" :: String)
            sqlSet "action_secondary_text_color" $ ("#ffffff" :: String)
            sqlSet "positive_color" $ ("#53b688" :: String)
            sqlSet "positive_text_color" $ ("#ffffff" :: String)
            sqlSet "negative_color" $ ("#b9322f" :: String)
            sqlSet "negative_text_color" $ ("#ffffff" :: String)
            sqlSet "font"
              $ ("\"Source Sans Pro\", \"Helvetica Neue\", Arial, sans-serif" :: String)
            sqlResult "id"
          (mailThemeId :: Int64) <- fetchOne runIdentity
          runQuery_ . sqlInsert "themes" $ do
            sqlSet "name" ("Scrive signing theme" :: String)
            sqlSet "logo" $ B64.decodeLenient $ BS.fromString $ lightTextLogo
            sqlSet "brand_color" $ ("#495259" :: String)
            sqlSet "brand_text_color" $ ("#ffffff" :: String)
            sqlSet "action_color" $ ("#53b688" :: String)
            sqlSet "action_text_color" $ ("#ffffff" :: String)
            sqlSet "action_secondary_color" $ ("#33b1dd" :: String)
            sqlSet "action_secondary_text_color" $ ("#ffffff" :: String)
            sqlSet "positive_color" $ ("#53b688" :: String)
            sqlSet "positive_text_color" $ ("#ffffff" :: String)
            sqlSet "negative_color" $ ("#b9322f" :: String)
            sqlSet "negative_text_color" $ ("#ffffff" :: String)
            sqlSet "font"
              $ ("\"Source Sans Pro\", \"Helvetica Neue\", Arial, sans-serif" :: String)
            sqlResult "id"
          (signviewThemeId :: Int64) <- fetchOne runIdentity
          runQuery_ . sqlInsert "themes" $ do
            sqlSet "name" ("Scrive service theme" :: String)
            sqlSet "logo" $ B64.decodeLenient $ BS.fromString $ lightTextLogo
            sqlSet "brand_color" $ ("#495259" :: String)
            sqlSet "brand_text_color" $ ("#ffffff" :: String)
            sqlSet "action_color" $ ("#53b688" :: String)
            sqlSet "action_text_color" $ ("#ffffff" :: String)
            sqlSet "action_secondary_color" $ ("#33b1dd" :: String)
            sqlSet "action_secondary_text_color" $ ("#ffffff" :: String)
            sqlSet "positive_color" $ ("#53b688" :: String)
            sqlSet "positive_text_color" $ ("#ffffff" :: String)
            sqlSet "negative_color" $ ("#b9322f" :: String)
            sqlSet "negative_text_color" $ ("#ffffff" :: String)
            sqlSet "font"
              $ ("\"Source Sans Pro\", \"Helvetica Neue\", Arial, sans-serif" :: String)
            sqlResult "id"
          (mainThemeId :: Int64) <- fetchOne runIdentity
          runQuery_ . sqlInsert "themes" $ do
            sqlSet "name" ("Scrive login theme" :: String)
            sqlSet "logo" $ B64.decodeLenient $ BS.fromString $ lightTextLogo
            sqlSet "brand_color" $ ("#495259" :: String)
            sqlSet "brand_text_color" $ ("#ffffff" :: String)
            sqlSet "action_color" $ ("#53b688" :: String)
            sqlSet "action_text_color" $ ("#ffffff" :: String)
            sqlSet "action_secondary_color" $ ("#33b1dd" :: String)
            sqlSet "action_secondary_text_color" $ ("#ffffff" :: String)
            sqlSet "positive_color" $ ("#53b688" :: String)
            sqlSet "positive_text_color" $ ("#ffffff" :: String)
            sqlSet "negative_color" $ ("#b9322f" :: String)
            sqlSet "negative_text_color" $ ("#ffffff" :: String)
            sqlSet "font"
              $ ("\"Source Sans Pro\", \"Helvetica Neue\", Arial, sans-serif" :: String)
            sqlResult "id"
          (loginThemeId :: Int64) <- fetchOne runIdentity
          runQuery_ . sqlInsert "branded_domains" $ do
            sqlSet "url"            ("https://scrive.com" :: String)
            sqlSet "mail_theme"     mailThemeId
            sqlSet "signview_theme" signviewThemeId
            sqlSet "service_theme"  mainThemeId
            sqlSet "login_theme"    loginThemeId
            sqlSet "main_domain" $ True
            sqlSet "sms_originator"   ("Scrive" :: String)
            sqlSet "email_originator" ("Scrive" :: String)
            sqlSet "browser_title"    ("Scrive" :: String)
            sqlSet "favicon" $ B64.decodeLenient $ BS.fromString $ favicon
            sqlSet "participant_color_1" ("#ff3377" :: String)
            sqlSet "participant_color_2" ("#009999" :: String)
            sqlSet "participant_color_3" ("#ffd700" :: String)
            sqlSet "participant_color_4" ("#7908aa" :: String)
            sqlSet "participant_color_5" ("#53df00" :: String)
            sqlSet "participant_color_6" ("#990000" :: String)
            sqlSet "draft_color"         ("#b2b2b2" :: String)
            sqlSet "cancelled_color"     ("#d64845" :: String)
            sqlSet "initiated_color"     ("#d2793a" :: String)
            sqlSet "sent_color"          ("#eca74d" :: String)
            sqlSet "delivered_color"     ("#e7d875" :: String)
            sqlSet "opened_color"        ("#54b588" :: String)
            sqlSet "reviewed_color"      ("#62c3de" :: String)
            sqlSet "signed_color"        ("#4c4c4c" :: String)
            sqlResult "id"
          (domainId :: Int64) <- fetchOne runIdentity
          runQuery_ . sqlInsert "theme_owners" $ do
            sqlSet "theme_id" mailThemeId
            sqlSet "domain_id" $ domainId
          runQuery_ . sqlInsert "theme_owners" $ do
            sqlSet "theme_id" signviewThemeId
            sqlSet "domain_id" $ domainId
          runQuery_ . sqlInsert "theme_owners" $ do
            sqlSet "theme_id" mainThemeId
            sqlSet "domain_id" $ domainId
          runQuery_ . sqlInsert "theme_owners" $ do
            sqlSet "theme_id" loginThemeId
            sqlSet "domain_id" $ domainId
      }
  }
