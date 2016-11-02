var _ = require("underscore");
var fs = require("fs");
var path = require("path");
var util = require("util");
var webpackConfig = require("./webpack.config.js");
var generateVersionId = require("./custom_grunt_tasks/utils/version_id_generator");
var langFromTexts = fs.readdirSync(path.join(__dirname, "../texts"));
var merge = require("webpack-merge");

// moment.js uses nn locale name for norwegian
langFromTexts = _.without(langFromTexts, "no");
langFromTexts.push("nn");

module.exports = function (grunt) {
  require("load-grunt-tasks")(grunt);
  require("time-grunt")(grunt);
  require("./custom_grunt_tasks/deploybuild")(grunt);
  grunt.loadNpmTasks("grunt-webpack");
  grunt.loadNpmTasks("grunt-contrib-uglify");

  var yeomanConfig = {
    app: require("./bower.json").appPath || "app",
    dist: "dist",
    kontrakcja: "../"
  };

  // Pick correct defaults when we're using new-build.
  var newBuild = fs.existsSync(yeomanConfig.kontrakcja + "dist-newstyle");
  var buildDir = "./dist";
  if (newBuild) {
      var spawnSync = require('child_process').spawnSync;
      var child = spawnSync("ghc", ["--numeric-version"]);
      var ghcVer = child.output[1].toString().trim();
      child = spawnSync(
          "ghc", ["-e", 'print $ System.Info.arch ++ "-" ++ System.Info.os'])
      var archOs = child.output[1].toString().trim();
      buildDir = "./dist-newstyle/build/" + archOs + "/ghc-"
          + ghcVer + "/kontrakcja-1.0/c/localization";
  }

  grunt.initConfig({
    yeoman: yeomanConfig,

    watch: {
      less: {
        files: ["<%= yeoman.app %>/less/**/*.less", "<%= yeoman.app %>/scripts/temporary_less_directory/**/*.less"],
        tasks: ["compileStyles"]
      },
      localization: {
        files: [
          "<%= yeoman.kontrakcja %>/texts/**/*.json",
          "<%= yeoman.kontrakcja %>/templates/javascript-langs.st"
        ],
        tasks: ["updateLocalization"]
      }
    },

    webpack: {
      all: webpackConfig[0],
      signview: webpackConfig[1],
      allWatch: merge({
        watch: true,
        keepalive: true
      }, webpackConfig[0]),
      signviewWatch: merge({
        watch: true,
        keepalive: true
      }, webpackConfig[1])
    },

    eslint: {
      src: require("./linter_manifest.json")
    },

    karma: {
      full: {
        singleRun: true,
        configFile: "karma.conf.js",
        autoWatch: false
      },
      fast: {
        singleRun: true,
        configFile: "karma.fast.conf.js",
        autoWatch: false
      }
    },

    clean: {
      dist: {
        files: [
          {
            dot: true,
            src: [
              ".tmp",
              "<%= yeoman.app %>/compiled/",
              "<%= yeoman.app %>/localization/",
              "<%= yeoman.dist %>/*",
              "!<%= yeoman.dist %>/.git*"
            ]
          }
        ]
      },
      server: {
        files: [
          {
            dot: true,
            src: [
              ".tmp",
              "<%= yeoman.app %>/compiled/"
            ]
          }
        ]
      }
    },

    deploybuild: {
      localization: {
        files: {
          src: [
            "<%= yeoman.app %>/localization/*.js"
          ]
        }
      },
      dist: {
        files: {
          src: [
            "<%= yeoman.dist %>/*.css"
          ]
        }
      }
    },

    shell: {
      options: {
        stdout: true,
        stderr: true,
        failOnError: true
      },
      updateLastBuilt: {
        command: "echo '$(date +%s)' > <%= yeoman.dist %>/LAST_BUILT"
      },
      compileLocalization: {
        command: (newBuild ? "cabal new-build" : "cabal build") + " localization",
        options: {
          execOptions: {
            cwd: "<%= yeoman.kontrakcja %>"
          }
        }
      },
      generateLocalization: {
        command: buildDir + "/build/localization/localization",
        options: {
          execOptions: {
            cwd: "<%= yeoman.kontrakcja %>"
          }
        }
      }
    },

    copy: {
      dist: {
        files: [
          {
            expand: true,
            dot: true,
            cwd: "<%= yeoman.app %>",
            dest: "<%= yeoman.dist %>",
            src: [
              "./*.{html,xml,jpg,png,ico,txt}",
              "api-explorer/**/*",
              "enable-cookies/**/*",
              "css/fonts/*.woff",
              "css/assets/*",
              "compiled/**/*",
              "fonts/**/*",
              "img/**/*",
              "newsletter/**/*",
              "pdf/**/*",
              "localization/*.*.js"
            ]
          }
        ]
      }
    },

    cssmin: {
      options: {
        report: true
      },
      dist: {
        files: {
          "<%= yeoman.dist %>/all-styling-minified.css": "<%= yeoman.app %>/less/less-compiled.css",
          "<%= yeoman.dist %>/signview-all-styling-minified.css": "<%= yeoman.app %>/less/signview-less-compiled.css"
        }
      }
    },

    concurrent: {
      dist: ["copy:dist"],
      watch: {
        tasks: ["watch", "webpack:allWatch", "webpack:signviewWatch"],
        options: {logConcurrentOutput: true}
      }
    },

    autoprefixer: {
      options: {
        browsers: ["ie >= 9", "chrome >= 31", "ff >= 38"]
      },
      compile: {
        src: "<%= yeoman.app %>/less/less-compiled.css"
      },
      signview: {
        src: "<%= yeoman.app %>/less/signview-less-compiled.css"
      }
    },

    uglify: {
      dev: {
        options: {
          sourceMap: true
        },
        src: [
          "<%= yeoman.app %>/bower_components/trackjs/tracker.js",
          "<%= yeoman.app %>/bower_components/jquery/dist/jquery.js",
          "<%= yeoman.app %>/bower_components/jquery-migrate/jquery-migrate.js",
          "<%= yeoman.app %>/bower_components/underscore/underscore.js",
          "<%= yeoman.app %>/bower_components/backbone/backbone.js",
          "<%= yeoman.app %>/bower_components/react/react-with-addons.js",
          "<%= yeoman.app %>/bower_components/moment/moment.js",
          util.format("<%= yeoman.app %>/bower_components/moment/locale/{%s}.js", langFromTexts.join(",")),
          "<%= yeoman.app %>/bower_components/es6-promise/promise.js",
          "<%= yeoman.app %>/bower_components/classnames/index.js",
          "<%= yeoman.app %>/libs/*.js",
          "<%= yeoman.app %>/js/global/cookie.js",
          "<%= yeoman.app %>/js/global/time.js",
          "<%= yeoman.app %>/js/global/shims.js",
          "<%= yeoman.app %>/js/global/errors.js",
          "<%= yeoman.app %>/js/global/csrf.js",
          "<%= yeoman.app %>/js/global/timezone.js"
        ],
        dest: "<%= yeoman.app %>/compiled/vendor-" + generateVersionId() + ".js"
      },
      prod: {
        src: [
          "<%= yeoman.app %>/bower_components/trackjs/tracker.js",
          "<%= yeoman.app %>/bower_components/jquery/dist/jquery.min.js",
          "<%= yeoman.app %>/bower_components/jquery-migrate/jquery-migrate.min.js",
          "<%= yeoman.app %>/bower_components/underscore/underscore-min.js",
          "<%= yeoman.app %>/bower_components/backbone/backbone-min.js",
          "<%= yeoman.app %>/bower_components/react/react-with-addons.min.js",
          "<%= yeoman.app %>/bower_components/moment/min/moment.min.js",
          util.format("<%= yeoman.app %>/bower_components/moment/locale/{%s}.js", langFromTexts.join(",")),
          "<%= yeoman.app %>/bower_components/es6-promise/promise.min.js",
          "<%= yeoman.app %>/bower_components/classnames/index.js",
          "<%= yeoman.app %>/libs/*.js",
          "<%= yeoman.app %>/js/global/cookie.js",
          "<%= yeoman.app %>/js/global/time.js",
          "<%= yeoman.app %>/js/global/shims.js",
          "<%= yeoman.app %>/js/global/errors.js",
          "<%= yeoman.app %>/js/global/csrf.js",
          "<%= yeoman.app %>/js/global/timezone.js"
        ],
        dest: "<%= yeoman.app %>/compiled/vendor-" + generateVersionId() + ".js"
      }
    },

    less: {
      options: {
        paths: ["<%= yeoman.app %>/less"],
        sourceMap: true
      },
      compile: {
        options: {
          sourceMapFilename: "<%= yeoman.app %>/less/less-compiled.css.map",
          sourceMapURL: "/less/less-compiled.css.map"
        },
        files: {
          "<%= yeoman.app %>/less/less-compiled.css": [
            "<%= yeoman.app %>/less/index.less",
            "<%= yeoman.app %>/css/*.css",
            "<%= yeoman.app %>/css/fonts/*.css"
          ]
        }
      },
      signview: {
        options: {
          sourceMapFilename: "<%= yeoman.app %>/less/signview-less-compiled.css.map",
          sourceMapURL: "/less/signview-less-compiled.css.map"
        },
        files: {
          "<%= yeoman.app %>/less/signview-less-compiled.css": [
            "<%= yeoman.app %>/less/signview/index.less",
            "<%= yeoman.app %>/css/fonts/*.css"
          ]
        }
      }
    }
  });

  grunt.registerTask("compileGenerateLocalization", function (target) {
    return grunt.task.run([
      "shell:compileLocalization",
      "shell:generateLocalization",
      "deploybuild:localization"
    ]);
  });

  grunt.registerTask("updateLocalization", function (target) {
    return grunt.task.run([
      "shell:generateLocalization",
      "deploybuild:localization"
    ]);
  });

  grunt.registerTask("buildJs", function (target) {
    var tasks = [
      "uglify:prod",
      "webpack:all",
      "webpack:signview"
    ];

    return grunt.task.run(tasks);
  });

  grunt.registerTask("build", function (target) {
    var tasks = [
      "clean:dist",
      "compileGenerateLocalization",
      "compileStyles",
      "buildJs",
      "cssmin:dist",
      "deploybuild:dist",
      "concurrent:dist",
      "shell:updateLastBuilt"
    ];

    return grunt.task.run(tasks);
  });

  grunt.registerTask("server", function (target) {
    grunt.task.run([
      "clean:server",
      "compileStyles",
      "compileGenerateLocalization",
      "uglify:dev",
      "concurrent:watch"
    ]);
  });

  grunt.registerTask("compileStyles", ["less", "autoprefixer"]);
  grunt.registerTask("server:dist", ["build"]);
  grunt.registerTask("test", ["eslint", "uglify:dev", "karma:full"]);
  grunt.registerTask("test:fast", ["uglify:dev", "karma:fast"]);
  grunt.registerTask("default", ["eslint", "build", "test"]);
};
