{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "2.4";
      identifier = { name = "kontrakcja"; version = "1.0"; };
      license = "NONE";
      copyright = "2010-2018";
      maintainer = "Scrive AB";
      author = "Scrive AB";
      homepage = "";
      url = "";
      synopsis = "Ultimate signing solution";
      description = "Sign documents online with the ultimate signing solution";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [
        "backend/flow/docs/*.yaml"
        "backend/flow/docs/*.md"
        "backend/flow/docs/*.py"
        "backend/flow/docs/*.html"
        "backend/flow/docs/*.txt"
        "backend/flow/docs/*.sh"
        "backend/flow/docs/*.pdf"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-noprelude" or (errorHandler.buildDepError "base-noprelude"))
          (hsPkgs."kontrakcja-prelude" or (errorHandler.buildDepError "kontrakcja-prelude"))
          (hsPkgs."kontrakcja-appdir" or (errorHandler.buildDepError "kontrakcja-appdir"))
          (hsPkgs."authentication" or (errorHandler.buildDepError "authentication"))
          (hsPkgs."kontrakcja-flow" or (errorHandler.buildDepError "kontrakcja-flow"))
          (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
          (hsPkgs."AES" or (errorHandler.buildDepError "AES"))
          (hsPkgs."DRBG" or (errorHandler.buildDepError "DRBG"))
          (hsPkgs."Decimal" or (errorHandler.buildDepError "Decimal"))
          (hsPkgs."HStringTemplate" or (errorHandler.buildDepError "HStringTemplate"))
          (hsPkgs."HTTP" or (errorHandler.buildDepError "HTTP"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-casing" or (errorHandler.buildDepError "aeson-casing"))
          (hsPkgs."amazonka" or (errorHandler.buildDepError "amazonka"))
          (hsPkgs."amazonka-core" or (errorHandler.buildDepError "amazonka-core"))
          (hsPkgs."amazonka-kinesis" or (errorHandler.buildDepError "amazonka-kinesis"))
          (hsPkgs."amazonka-s3" or (errorHandler.buildDepError "amazonka-s3"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."blaze-markup" or (errorHandler.buildDepError "blaze-markup"))
          (hsPkgs."blaze-html" or (errorHandler.buildDepError "blaze-html"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."cond" or (errorHandler.buildDepError "cond"))
          (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
          (hsPkgs."consumers" or (errorHandler.buildDepError "consumers"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cookie" or (errorHandler.buildDepError "cookie"))
          (hsPkgs."crypto-rng" or (errorHandler.buildDepError "crypto-rng"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."cryptostore" or (errorHandler.buildDepError "cryptostore"))
          (hsPkgs."curl" or (errorHandler.buildDepError "curl"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."digest" or (errorHandler.buildDepError "digest"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."Diff" or (errorHandler.buildDepError "Diff"))
          (hsPkgs."either" or (errorHandler.buildDepError "either"))
          (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
          (hsPkgs."ekg-statsd" or (errorHandler.buildDepError "ekg-statsd"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."explicit-exception" or (errorHandler.buildDepError "explicit-exception"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."optics" or (errorHandler.buildDepError "optics"))
          (hsPkgs."fields-json" or (errorHandler.buildDepError "fields-json"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."free" or (errorHandler.buildDepError "free"))
          (hsPkgs."happstack-server" or (errorHandler.buildDepError "happstack-server"))
          (hsPkgs."happstack-static-routing" or (errorHandler.buildDepError "happstack-static-routing"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."hedis" or (errorHandler.buildDepError "hedis"))
          (hsPkgs."hpqtypes" or (errorHandler.buildDepError "hpqtypes"))
          (hsPkgs."hpqtypes-extras" or (errorHandler.buildDepError "hpqtypes-extras"))
          (hsPkgs."hsaml2" or (errorHandler.buildDepError "hsaml2"))
          (hsPkgs."hxt" or (errorHandler.buildDepError "hxt"))
          (hsPkgs."hxt-http" or (errorHandler.buildDepError "hxt-http"))
          (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
          (hsPkgs."http-conduit" or (errorHandler.buildDepError "http-conduit"))
          (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
          (hsPkgs."http-media" or (errorHandler.buildDepError "http-media"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."iconv" or (errorHandler.buildDepError "iconv"))
          (hsPkgs."invariant" or (errorHandler.buildDepError "invariant"))
          (hsPkgs."json" or (errorHandler.buildDepError "json"))
          (hsPkgs."JuicyPixels" or (errorHandler.buildDepError "JuicyPixels"))
          (hsPkgs."kontrakcja-templates" or (errorHandler.buildDepError "kontrakcja-templates"))
          (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
          (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
          (hsPkgs."lifted-threads" or (errorHandler.buildDepError "lifted-threads"))
          (hsPkgs."log-base" or (errorHandler.buildDepError "log-base"))
          (hsPkgs."log-elasticsearch" or (errorHandler.buildDepError "log-elasticsearch"))
          (hsPkgs."log-postgres" or (errorHandler.buildDepError "log-postgres"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."mixpanel" or (errorHandler.buildDepError "mixpanel"))
          (hsPkgs."mmap" or (errorHandler.buildDepError "mmap"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."monad-time" or (errorHandler.buildDepError "monad-time"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."network-bsd" or (errorHandler.buildDepError "network-bsd"))
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          (hsPkgs."one-time-password" or (errorHandler.buildDepError "one-time-password"))
          (hsPkgs."pem" or (errorHandler.buildDepError "pem"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."process-extras" or (errorHandler.buildDepError "process-extras"))
          (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
          (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."raw-strings-qq" or (errorHandler.buildDepError "raw-strings-qq"))
          (hsPkgs."regex-tdfa" or (errorHandler.buildDepError "regex-tdfa"))
          (hsPkgs."resource-pool" or (errorHandler.buildDepError "resource-pool"))
          (hsPkgs."sandi" or (errorHandler.buildDepError "sandi"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."scrypt" or (errorHandler.buildDepError "scrypt"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."servant-auth" or (errorHandler.buildDepError "servant-auth"))
          (hsPkgs."servant-blaze" or (errorHandler.buildDepError "servant-blaze"))
          (hsPkgs."servant-checked-exceptions" or (errorHandler.buildDepError "servant-checked-exceptions"))
          (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
          (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
          (hsPkgs."servant-errors" or (errorHandler.buildDepError "servant-errors"))
          (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
          (hsPkgs."soap" or (errorHandler.buildDepError "soap"))
          (hsPkgs."split" or (errorHandler.buildDepError "split"))
          (hsPkgs."spreadsheet" or (errorHandler.buildDepError "spreadsheet"))
          (hsPkgs."statistics" or (errorHandler.buildDepError "statistics"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."tagsoup" or (errorHandler.buildDepError "tagsoup"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."text-icu" or (errorHandler.buildDepError "text-icu"))
          (hsPkgs."text-show" or (errorHandler.buildDepError "text-show"))
          (hsPkgs."threads" or (errorHandler.buildDepError "threads"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
          (hsPkgs."unjson" or (errorHandler.buildDepError "unjson"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
          (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
          (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
          (hsPkgs."wai-app-static" or (errorHandler.buildDepError "wai-app-static"))
          (hsPkgs."wai-log" or (errorHandler.buildDepError "wai-log"))
          (hsPkgs."x509" or (errorHandler.buildDepError "x509"))
          (hsPkgs."xml-conduit" or (errorHandler.buildDepError "xml-conduit"))
          (hsPkgs."xml-conduit-writer" or (errorHandler.buildDepError "xml-conduit-writer"))
          (hsPkgs."xml-types" or (errorHandler.buildDepError "xml-types"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          (hsPkgs."zip-archive" or (errorHandler.buildDepError "zip-archive"))
          ];
        pkgconfig = [
          (pkgconfPkgs."libxml-2.0" or (errorHandler.pkgConfDepError "libxml-2.0"))
          ];
        buildable = true;
        hsSourceDirs = [
          "backend/cron/inc"
          "backend/cron/schema"
          "backend/lib"
          "backend/mailer/inc"
          "backend/mailer/schema"
          "backend/mailer/src"
          "backend/messenger/inc"
          "backend/messenger/schema"
          "backend/messenger/src"
          "backend/migrate/inc"
          "backend/misc/schema"
          "backend/misc/src"
          "backend/server/schema"
          "backend/server/src"
          "backend/flow/src"
          ];
        };
      sublibs = {
        "kontrakcja-prelude" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cond" or (errorHandler.buildDepError "cond"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."optics" or (errorHandler.buildDepError "optics"))
            (hsPkgs."hpqtypes" or (errorHandler.buildDepError "hpqtypes"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."text-show" or (errorHandler.buildDepError "text-show"))
            (hsPkgs."fields-json" or (errorHandler.buildDepError "fields-json"))
            ];
          buildable = true;
          hsSourceDirs = [ "backend/kontrakcja-prelude" ];
          };
        "kontrakcja-appdir" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ];
          buildable = true;
          hsSourceDirs = [ "backend/appdir" ];
          };
        "kontrakcja-flow" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-noprelude" or (errorHandler.buildDepError "base-noprelude"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."aeson-casing" or (errorHandler.buildDepError "aeson-casing"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."hpqtypes" or (errorHandler.buildDepError "hpqtypes"))
            (hsPkgs."kontrakcja-prelude" or (errorHandler.buildDepError "kontrakcja-prelude"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."optics" or (errorHandler.buildDepError "optics"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            ];
          buildable = true;
          hsSourceDirs = [ "backend/flow/lib" ];
          };
        "authentication" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-noprelude" or (errorHandler.buildDepError "base-noprelude"))
            (hsPkgs."kontrakcja-prelude" or (errorHandler.buildDepError "kontrakcja-prelude"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cond" or (errorHandler.buildDepError "cond"))
            (hsPkgs."crypto-rng" or (errorHandler.buildDepError "crypto-rng"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."happstack-server" or (errorHandler.buildDepError "happstack-server"))
            (hsPkgs."hpqtypes" or (errorHandler.buildDepError "hpqtypes"))
            (hsPkgs."hpqtypes-extras" or (errorHandler.buildDepError "hpqtypes-extras"))
            (hsPkgs."http-api-data" or (errorHandler.buildDepError "http-api-data"))
            (hsPkgs."monad-time" or (errorHandler.buildDepError "monad-time"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."text-show" or (errorHandler.buildDepError "text-show"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."unjson" or (errorHandler.buildDepError "unjson"))
            (hsPkgs."HTTP" or (errorHandler.buildDepError "HTTP"))
            ];
          buildable = true;
          hsSourceDirs = [ "backend/authentication/src" ];
          };
        };
      exes = {
        "kontrakcja-server" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-noprelude" or (errorHandler.buildDepError "base-noprelude"))
            (hsPkgs."kontrakcja-prelude" or (errorHandler.buildDepError "kontrakcja-prelude"))
            (hsPkgs."kontrakcja-appdir" or (errorHandler.buildDepError "kontrakcja-appdir"))
            (hsPkgs."kontrakcja" or (errorHandler.buildDepError "kontrakcja"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cmdargs" or (errorHandler.buildDepError "cmdargs"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."crypto-rng" or (errorHandler.buildDepError "crypto-rng"))
            (hsPkgs."curl" or (errorHandler.buildDepError "curl"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."kontrakcja-flow" or (errorHandler.buildDepError "kontrakcja-flow"))
            (hsPkgs."happstack-server" or (errorHandler.buildDepError "happstack-server"))
            (hsPkgs."happstack-static-routing" or (errorHandler.buildDepError "happstack-static-routing"))
            (hsPkgs."hedis" or (errorHandler.buildDepError "hedis"))
            (hsPkgs."hostname" or (errorHandler.buildDepError "hostname"))
            (hsPkgs."hpqtypes" or (errorHandler.buildDepError "hpqtypes"))
            (hsPkgs."hpqtypes-extras" or (errorHandler.buildDepError "hpqtypes-extras"))
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            (hsPkgs."log-base" or (errorHandler.buildDepError "log-base"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            ];
          buildable = true;
          hsSourceDirs = [ "backend/server/src" ];
          mainPath = [ "KontrakcjaServerMain.hs" ];
          };
        "cron" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-noprelude" or (errorHandler.buildDepError "base-noprelude"))
            (hsPkgs."kontrakcja" or (errorHandler.buildDepError "kontrakcja"))
            (hsPkgs."kontrakcja-appdir" or (errorHandler.buildDepError "kontrakcja-appdir"))
            (hsPkgs."kontrakcja-prelude" or (errorHandler.buildDepError "kontrakcja-prelude"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cmdargs" or (errorHandler.buildDepError "cmdargs"))
            (hsPkgs."consumers" or (errorHandler.buildDepError "consumers"))
            (hsPkgs."crypto-rng" or (errorHandler.buildDepError "crypto-rng"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hedis" or (errorHandler.buildDepError "hedis"))
            (hsPkgs."hostname" or (errorHandler.buildDepError "hostname"))
            (hsPkgs."hpqtypes" or (errorHandler.buildDepError "hpqtypes"))
            (hsPkgs."hpqtypes-extras" or (errorHandler.buildDepError "hpqtypes-extras"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
            (hsPkgs."http-conduit" or (errorHandler.buildDepError "http-conduit"))
            (hsPkgs."log-base" or (errorHandler.buildDepError "log-base"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."unjson" or (errorHandler.buildDepError "unjson"))
            ];
          buildable = true;
          hsSourceDirs = [ "backend/cron/src" ];
          mainPath = [ "CronMain.hs" ];
          };
        "kontrakcja-migrate" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-noprelude" or (errorHandler.buildDepError "base-noprelude"))
            (hsPkgs."kontrakcja" or (errorHandler.buildDepError "kontrakcja"))
            (hsPkgs."kontrakcja-appdir" or (errorHandler.buildDepError "kontrakcja-appdir"))
            (hsPkgs."kontrakcja-prelude" or (errorHandler.buildDepError "kontrakcja-prelude"))
            (hsPkgs."DRBG" or (errorHandler.buildDepError "DRBG"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cmdargs" or (errorHandler.buildDepError "cmdargs"))
            (hsPkgs."cond" or (errorHandler.buildDepError "cond"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."crypto-api" or (errorHandler.buildDepError "crypto-api"))
            (hsPkgs."crypto-rng" or (errorHandler.buildDepError "crypto-rng"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."optics" or (errorHandler.buildDepError "optics"))
            (hsPkgs."fields-json" or (errorHandler.buildDepError "fields-json"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hpqtypes" or (errorHandler.buildDepError "hpqtypes"))
            (hsPkgs."hpqtypes-extras" or (errorHandler.buildDepError "hpqtypes-extras"))
            (hsPkgs."invariant" or (errorHandler.buildDepError "invariant"))
            (hsPkgs."json" or (errorHandler.buildDepError "json"))
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            (hsPkgs."log-base" or (errorHandler.buildDepError "log-base"))
            (hsPkgs."log-elasticsearch" or (errorHandler.buildDepError "log-elasticsearch"))
            (hsPkgs."log-postgres" or (errorHandler.buildDepError "log-postgres"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."monad-time" or (errorHandler.buildDepError "monad-time"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."regex-tdfa" or (errorHandler.buildDepError "regex-tdfa"))
            (hsPkgs."resource-pool" or (errorHandler.buildDepError "resource-pool"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."tagsoup" or (errorHandler.buildDepError "tagsoup"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."unjson" or (errorHandler.buildDepError "unjson"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            ];
          buildable = true;
          hsSourceDirs = [ "backend/migrate/src" ];
          mainPath = [ "KontrakcjaMigrateMain.hs" ];
          };
        "mailing-server" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-noprelude" or (errorHandler.buildDepError "base-noprelude"))
            (hsPkgs."kontrakcja" or (errorHandler.buildDepError "kontrakcja"))
            (hsPkgs."kontrakcja-appdir" or (errorHandler.buildDepError "kontrakcja-appdir"))
            (hsPkgs."kontrakcja-prelude" or (errorHandler.buildDepError "kontrakcja-prelude"))
            (hsPkgs."authentication" or (errorHandler.buildDepError "authentication"))
            (hsPkgs."AES" or (errorHandler.buildDepError "AES"))
            (hsPkgs."DRBG" or (errorHandler.buildDepError "DRBG"))
            (hsPkgs."HTTP" or (errorHandler.buildDepError "HTTP"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."amazonka" or (errorHandler.buildDepError "amazonka"))
            (hsPkgs."amazonka-core" or (errorHandler.buildDepError "amazonka-core"))
            (hsPkgs."amazonka-s3" or (errorHandler.buildDepError "amazonka-s3"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cmdargs" or (errorHandler.buildDepError "cmdargs"))
            (hsPkgs."cond" or (errorHandler.buildDepError "cond"))
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."consumers" or (errorHandler.buildDepError "consumers"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."crypto-api" or (errorHandler.buildDepError "crypto-api"))
            (hsPkgs."crypto-rng" or (errorHandler.buildDepError "crypto-rng"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."hostname" or (errorHandler.buildDepError "hostname"))
            (hsPkgs."hpqtypes-extras" or (errorHandler.buildDepError "hpqtypes-extras"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
            (hsPkgs."ekg-statsd" or (errorHandler.buildDepError "ekg-statsd"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."optics" or (errorHandler.buildDepError "optics"))
            (hsPkgs."fields-json" or (errorHandler.buildDepError "fields-json"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."happstack-server" or (errorHandler.buildDepError "happstack-server"))
            (hsPkgs."happstack-static-routing" or (errorHandler.buildDepError "happstack-static-routing"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."hedis" or (errorHandler.buildDepError "hedis"))
            (hsPkgs."hpqtypes" or (errorHandler.buildDepError "hpqtypes"))
            (hsPkgs."invariant" or (errorHandler.buildDepError "invariant"))
            (hsPkgs."json" or (errorHandler.buildDepError "json"))
            (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            (hsPkgs."log-base" or (errorHandler.buildDepError "log-base"))
            (hsPkgs."log-elasticsearch" or (errorHandler.buildDepError "log-elasticsearch"))
            (hsPkgs."log-postgres" or (errorHandler.buildDepError "log-postgres"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."monad-time" or (errorHandler.buildDepError "monad-time"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network-bsd" or (errorHandler.buildDepError "network-bsd"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."process-extras" or (errorHandler.buildDepError "process-extras"))
            (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
            (hsPkgs."punycode" or (errorHandler.buildDepError "punycode"))
            (hsPkgs."resource-pool" or (errorHandler.buildDepError "resource-pool"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."tagsoup" or (errorHandler.buildDepError "tagsoup"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."unjson" or (errorHandler.buildDepError "unjson"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            ];
          buildable = true;
          hsSourceDirs = [ "backend/mailer/src" ];
          mainPath = [ "MailingServerMain.hs" ];
          };
        "messenger-server" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-noprelude" or (errorHandler.buildDepError "base-noprelude"))
            (hsPkgs."kontrakcja" or (errorHandler.buildDepError "kontrakcja"))
            (hsPkgs."kontrakcja-appdir" or (errorHandler.buildDepError "kontrakcja-appdir"))
            (hsPkgs."kontrakcja-prelude" or (errorHandler.buildDepError "kontrakcja-prelude"))
            (hsPkgs."DRBG" or (errorHandler.buildDepError "DRBG"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cmdargs" or (errorHandler.buildDepError "cmdargs"))
            (hsPkgs."cond" or (errorHandler.buildDepError "cond"))
            (hsPkgs."consumers" or (errorHandler.buildDepError "consumers"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."crypto-rng" or (errorHandler.buildDepError "crypto-rng"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."hpqtypes-extras" or (errorHandler.buildDepError "hpqtypes-extras"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
            (hsPkgs."ekg-statsd" or (errorHandler.buildDepError "ekg-statsd"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."optics" or (errorHandler.buildDepError "optics"))
            (hsPkgs."fields-json" or (errorHandler.buildDepError "fields-json"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."happstack-server" or (errorHandler.buildDepError "happstack-server"))
            (hsPkgs."happstack-static-routing" or (errorHandler.buildDepError "happstack-static-routing"))
            (hsPkgs."hostname" or (errorHandler.buildDepError "hostname"))
            (hsPkgs."hpqtypes" or (errorHandler.buildDepError "hpqtypes"))
            (hsPkgs."HTTP" or (errorHandler.buildDepError "HTTP"))
            (hsPkgs."iconv" or (errorHandler.buildDepError "iconv"))
            (hsPkgs."invariant" or (errorHandler.buildDepError "invariant"))
            (hsPkgs."json" or (errorHandler.buildDepError "json"))
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            (hsPkgs."log-base" or (errorHandler.buildDepError "log-base"))
            (hsPkgs."log-elasticsearch" or (errorHandler.buildDepError "log-elasticsearch"))
            (hsPkgs."log-postgres" or (errorHandler.buildDepError "log-postgres"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."monad-time" or (errorHandler.buildDepError "monad-time"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network-bsd" or (errorHandler.buildDepError "network-bsd"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."process-extras" or (errorHandler.buildDepError "process-extras"))
            (hsPkgs."regex-tdfa" or (errorHandler.buildDepError "regex-tdfa"))
            (hsPkgs."resource-pool" or (errorHandler.buildDepError "resource-pool"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."tagsoup" or (errorHandler.buildDepError "tagsoup"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."unjson" or (errorHandler.buildDepError "unjson"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            ];
          buildable = true;
          hsSourceDirs = [ "backend/messenger/src" ];
          mainPath = [ "MessengerServerMain.hs" ];
          };
        "config-checker" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-noprelude" or (errorHandler.buildDepError "base-noprelude"))
            (hsPkgs."kontrakcja-appdir" or (errorHandler.buildDepError "kontrakcja-appdir"))
            (hsPkgs."kontrakcja" or (errorHandler.buildDepError "kontrakcja"))
            (hsPkgs."kontrakcja-prelude" or (errorHandler.buildDepError "kontrakcja-prelude"))
            (hsPkgs."aeson-diff" or (errorHandler.buildDepError "aeson-diff"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."either" or (errorHandler.buildDepError "either"))
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unjson" or (errorHandler.buildDepError "unjson"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ];
          buildable = true;
          hsSourceDirs = [
            "backend/config-checker/src"
            "backend/cron/inc"
            "backend/mailer/src"
            "backend/messenger/src"
            ];
          mainPath = [ "ConfigCheckerMain.hs" ];
          };
        "localization" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-noprelude" or (errorHandler.buildDepError "base-noprelude"))
            (hsPkgs."kontrakcja-prelude" or (errorHandler.buildDepError "kontrakcja-prelude"))
            (hsPkgs."kontrakcja-appdir" or (errorHandler.buildDepError "kontrakcja-appdir"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."cond" or (errorHandler.buildDepError "cond"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."optics" or (errorHandler.buildDepError "optics"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."happstack-server" or (errorHandler.buildDepError "happstack-server"))
            (hsPkgs."hpqtypes" or (errorHandler.buildDepError "hpqtypes"))
            (hsPkgs."kontrakcja-templates" or (errorHandler.buildDepError "kontrakcja-templates"))
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          hsSourceDirs = [
            "localization/src"
            "backend/lib"
            "backend/misc/src"
            ];
          mainPath = [ "LocalizationMain.hs" ];
          };
        "routinglist" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-noprelude" or (errorHandler.buildDepError "base-noprelude"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."kontrakcja" or (errorHandler.buildDepError "kontrakcja"))
            (hsPkgs."kontrakcja-appdir" or (errorHandler.buildDepError "kontrakcja-appdir"))
            (hsPkgs."kontrakcja-prelude" or (errorHandler.buildDepError "kontrakcja-prelude"))
            (hsPkgs."happstack-server" or (errorHandler.buildDepError "happstack-server"))
            (hsPkgs."happstack-static-routing" or (errorHandler.buildDepError "happstack-static-routing"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ];
          buildable = true;
          hsSourceDirs = [ "backend/routinglist/src" ];
          mainPath = [ "RoutinglistMain.hs" ];
          };
        "transifex" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."fields-json" or (errorHandler.buildDepError "fields-json"))
            (hsPkgs."json" or (errorHandler.buildDepError "json"))
            (hsPkgs."kontrakcja-appdir" or (errorHandler.buildDepError "kontrakcja-appdir"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
            (hsPkgs."old-time" or (errorHandler.buildDepError "old-time"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            ];
          buildable = true;
          hsSourceDirs = [ "transifex/src" ];
          mainPath = [ "TransifexMain.hs" ];
          };
        "detect_old_localizations" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."kontrakcja-appdir" or (errorHandler.buildDepError "kontrakcja-appdir"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."language-javascript" or (errorHandler.buildDepError "language-javascript"))
            (hsPkgs."regex-tdfa" or (errorHandler.buildDepError "regex-tdfa"))
            ];
          buildable = true;
          hsSourceDirs = [ "scripts/detect_old_localizations" ];
          mainPath = [ "DetectOldLocalizationsMain.hs" ];
          };
        "detect_old_templates" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."kontrakcja-appdir" or (errorHandler.buildDepError "kontrakcja-appdir"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."fields-json" or (errorHandler.buildDepError "fields-json"))
            (hsPkgs."haskell-src-exts" or (errorHandler.buildDepError "haskell-src-exts"))
            (hsPkgs."HStringTemplate" or (errorHandler.buildDepError "HStringTemplate"))
            (hsPkgs."json" or (errorHandler.buildDepError "json"))
            (hsPkgs."kontrakcja-templates" or (errorHandler.buildDepError "kontrakcja-templates"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."old-time" or (errorHandler.buildDepError "old-time"))
            (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
            ];
          buildable = true;
          hsSourceDirs = [ "scripts/detect_old_templates" "transifex/src" ];
          mainPath = [ "DetectOldTemplatesMain.hs" ];
          };
        "sort_imports" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."kontrakcja-appdir" or (errorHandler.buildDepError "kontrakcja-appdir"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          hsSourceDirs = [ "scripts/sort_imports" ];
          mainPath = [ "SortImportsMain.hs" ];
          };
        };
      tests = {
        "kontrakcja-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-noprelude" or (errorHandler.buildDepError "base-noprelude"))
            (hsPkgs."kontrakcja-prelude" or (errorHandler.buildDepError "kontrakcja-prelude"))
            (hsPkgs."kontrakcja-appdir" or (errorHandler.buildDepError "kontrakcja-appdir"))
            (hsPkgs."kontrakcja" or (errorHandler.buildDepError "kontrakcja"))
            (hsPkgs."kontrakcja-flow" or (errorHandler.buildDepError "kontrakcja-flow"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."aeson-diff" or (errorHandler.buildDepError "aeson-diff"))
            (hsPkgs."amazonka" or (errorHandler.buildDepError "amazonka"))
            (hsPkgs."authentication" or (errorHandler.buildDepError "authentication"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."blaze-markup" or (errorHandler.buildDepError "blaze-markup"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cond" or (errorHandler.buildDepError "cond"))
            (hsPkgs."consumers" or (errorHandler.buildDepError "consumers"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."cookie" or (errorHandler.buildDepError "cookie"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."crypto-rng" or (errorHandler.buildDepError "crypto-rng"))
            (hsPkgs."cryptostore" or (errorHandler.buildDepError "cryptostore"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."Decimal" or (errorHandler.buildDepError "Decimal"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."either" or (errorHandler.buildDepError "either"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."fields-json" or (errorHandler.buildDepError "fields-json"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."free" or (errorHandler.buildDepError "free"))
            (hsPkgs."happstack-server" or (errorHandler.buildDepError "happstack-server"))
            (hsPkgs."hedis" or (errorHandler.buildDepError "hedis"))
            (hsPkgs."hpqtypes" or (errorHandler.buildDepError "hpqtypes"))
            (hsPkgs."hpqtypes-extras" or (errorHandler.buildDepError "hpqtypes-extras"))
            (hsPkgs."hsaml2" or (errorHandler.buildDepError "hsaml2"))
            (hsPkgs."HStringTemplate" or (errorHandler.buildDepError "HStringTemplate"))
            (hsPkgs."HTTP" or (errorHandler.buildDepError "HTTP"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."hxt" or (errorHandler.buildDepError "hxt"))
            (hsPkgs."invariant" or (errorHandler.buildDepError "invariant"))
            (hsPkgs."json" or (errorHandler.buildDepError "json"))
            (hsPkgs."kontrakcja-templates" or (errorHandler.buildDepError "kontrakcja-templates"))
            (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            (hsPkgs."log-base" or (errorHandler.buildDepError "log-base"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."monad-loops" or (errorHandler.buildDepError "monad-loops"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."one-time-password" or (errorHandler.buildDepError "one-time-password"))
            (hsPkgs."optics" or (errorHandler.buildDepError "optics"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."quickcheck-unicode" or (errorHandler.buildDepError "quickcheck-unicode"))
            (hsPkgs."raw-strings-qq" or (errorHandler.buildDepError "raw-strings-qq"))
            (hsPkgs."regex-tdfa" or (errorHandler.buildDepError "regex-tdfa"))
            (hsPkgs."sandi" or (errorHandler.buildDepError "sandi"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."scrypt" or (errorHandler.buildDepError "scrypt"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-blaze" or (errorHandler.buildDepError "servant-blaze"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."tagsoup" or (errorHandler.buildDepError "tagsoup"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."text-icu" or (errorHandler.buildDepError "text-icu"))
            (hsPkgs."threads" or (errorHandler.buildDepError "threads"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."unjson" or (errorHandler.buildDepError "unjson"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            (hsPkgs."x509" or (errorHandler.buildDepError "x509"))
            (hsPkgs."xml-conduit" or (errorHandler.buildDepError "xml-conduit"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            ];
          buildable = true;
          hsSourceDirs = [ "backend/test/src" ];
          mainPath = [ "TestMain.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }