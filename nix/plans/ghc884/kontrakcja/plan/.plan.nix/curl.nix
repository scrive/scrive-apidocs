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
    flags = { new-base = false; };
    package = {
      specVersion = "1.6";
      identifier = { name = "curl"; version = "1.3.8.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Don Stewart <dons00@gmail.com>";
      author = "Sigbjorn Finne";
      homepage = "";
      url = "";
      synopsis = "Haskell binding to libcurl";
      description = "libcurl is a client-side URL transfer library, supporting FTP, FTPS, HTTP,\nHTTPS, SCP, SFTP, TFTP, TELNET, DICT, LDAP, LDAPS and FILE.\nlibcurl supports SSL certificates, HTTP POST, HTTP PUT, FTP uploading,\nHTTP form based upload, proxies, cookies, user+password authentication\n(Basic, Digest, NTLM, Negotiate, Kerberos4), file transfer resume,\nhttp proxy tunneling and more!\n\nThis package provides a Haskell binding to libcurl.";
      buildType = "Configure";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [
        "configure"
        "configure.ac"
        "curl.buildinfo.in"
        "CHANGES"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ] ++ (if flags.new-base
          then [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            ]
          else [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ]);
        libs = [ (pkgs."curl" or (errorHandler.sysDepError "curl")) ];
        buildable = true;
        modules = [
          "Network/Curl"
          "Network/Curl/Code"
          "Network/Curl/Info"
          "Network/Curl/Opts"
          "Network/Curl/Post"
          "Network/Curl/Types"
          "Network/Curl/Easy"
          "Network/Curl/Debug"
          ];
        cSources = [ "curlc.c" ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././.source-repository-packages/4; }