{ nixpkgs }:
let
  version = "380d64698fa24a898ac34f8f8644c9548dbf6211";
  sha256 = "13v1a17w1fsdwpr1kaip47r2l1f9g4icksif36icsn0d14ak0853";
  sam-src = builtins.fetchTarball
    { inherit sha256;
      url = "https://github.com/aws/aws-sam-cli/archive/${version}.tar.gz";
    };

  python = nixpkgs.python37.override {
    packageOverrides = self: super: {
      jmespath = super.jmespath.overridePythonAttrs (oldAttrs: rec {
        version = "0.9.5";
        src = oldAttrs.src.override {
          inherit version;
          sha256 = "1nf2ipzvigspy17r16dpkhzn1bqdmlak162rm8dy4wri2n6mr9fc";
        };
      });

      cookiecutter = super.cookiecutter.overridePythonAttrs (oldAttrs: rec {
        version = "1.6.0";
        src = oldAttrs.src.override {
          inherit version;
          sha256 = "0glsvaz8igi2wy1hsnhm9fkn6560vdvdixzvkq6dn20z3hpaa5hk";
        };
      });

      flask = super.flask.overridePythonAttrs (oldAttrs: rec {
        version = "1.0.2";
        src = oldAttrs.src.override {
          inherit version;
          sha256 = "0j6f4a9rpfh25k1gp7azqhnni4mb4fgy50jammgjgddw1l3w0w92";
        };
      });

      tomlkit = super.tomlkit.overridePythonAttrs (oldAttrs: rec {
        version = "0.5.8";
        src = oldAttrs.src.override {
          inherit version;
          sha256 = "0sf2a4q61kf344hjbw8kb6za1hlccl89j9lzqw0l2zpddp0hrh9j";
        };
      });

      aws-sam-translator = super.aws-sam-translator.overridePythonAttrs (oldAttrs: rec {
        version = "1.25.0";
        src = oldAttrs.src.override {
          inherit version;
          sha256 = "08756yl5lpqgrpr80f2b6bdcgygr37l6q1yygklcg9hz4yfpccav";
        };
      });

      boto3 = super.boto3.overridePythonAttrs (oldAttrs: rec {
        version = "1.13.0";
        src = oldAttrs.src.override {
          inherit version;
          sha256 = "0wpzvlxiyhm0m125x2kqdv24f3gaqs39xgwmhb3yb52nh3xnqnc0";
        };
      });

      botocore = super.botocore.overridePythonAttrs (oldAttrs: rec {
        version = "1.16.0";
        src = oldAttrs.src.override {
          inherit version;
          sha256 = "1gxm6ja31xx7yzkql84hy5s4flvr2y0w17sxkgn6q7jmgmfnnrsb";
        };
      });

      python-dateutil = super.python-dateutil.overridePythonAttrs (oldAttrs: rec {
        version = "2.7.0";
        src = oldAttrs.src.override {
          inherit version;
          sha256 = "0073wspqialh5kkcli2q9x5h4hm9vk45iyx1a5m4bcnvdrzbp5cg";
        };
      });

      requests = super.requests.overridePythonAttrs (oldAttrs: rec {
        version = "2.23.0";
        src = oldAttrs.src.override {
          inherit version;
          sha256 = "1rhpg0jb08v0gd7f19jjiwlcdnxpmqi1fhvw7r4s9avddi4kvx5k";
        };
      });

      serverlessrepo = super.serverlessrepo.overridePythonAttrs (oldAttrs: rec {
        version = "0.1.9";
        src = oldAttrs.src.override {
          inherit version;
          sha256 = "1xf0g97jym4607kikkiassrnmcfniz5syaigxlz09d9p8h70sd0c";
        };
      });
    };
  };

  inherit (python) pkgs;
in
pkgs.buildPythonApplication rec {
  pname = "aws-sam-cli";
  version = "0.53.0";

  src = sam-src;

  propagatedBuildInputs = [
    pkgs.jmespath
    pkgs.cookiecutter
    pkgs.flask
    pkgs.docker
    pkgs.tomlkit
    pkgs.chevron
    pkgs.aws-sam-translator
    pkgs.boto3
    pkgs.botocore
    pkgs.aws-lambda-builders
    pkgs.dateparser
    pkgs.pyyaml
    pkgs.dateparser
    pkgs.python-dateutil
    pkgs.requests
    pkgs.serverlessrepo
    pkgs.click
  ];

  doCheck = false;
}
