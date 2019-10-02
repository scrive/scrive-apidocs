var _ = require("underscore");
var fs = require("fs");
var path = require("path");
var util = require("util");
var webpack = require("webpack");
var webpackConfig = require("./webpack.config.js");
var generateVersionId = require("./custom_grunt_tasks/utils/version_id_generator");
var langFromTexts = fs.readdirSync(path.join(__dirname, "../texts"));
var merge = require("webpack-merge");

// moment.js uses nn locale name for norwegian
langFromTexts = _.without(langFromTexts, "no");
langFromTexts.push("nn");

const sourceDir = process.env.KONTRAKCJA_ROOT || "../";
const workspaceDir = process.env.KONTRAKCJA_WORKSPACE || "../";

module.exports = function (grunt) {
  require("load-grunt-tasks")(grunt);
  require("time-grunt")(grunt);
  require("./custom_grunt_tasks/deploybuild")(grunt);
  grunt.loadNpmTasks("grunt-webpack");
  grunt.loadNpmTasks("grunt-contrib-uglify");

  var yeomanConfig = {
    app: require("./bower.json").appPath || "app",
    dist: "dist",
    kontrakcjaRoot: sourceDir,
    kontrakcjaWorkspace: workspaceDir
  };

  // Pick correct defaults when we're using Haskell's 'cabal new-build'.
  var newBuild;
  if (grunt.option("no-new-build")) {
      newBuild = false;
  } else {
      newBuild = grunt.option("new-build")
          || fs.existsSync(yeomanConfig.kontrakcjaWorkspace + "dist-newstyle");
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
          "<%= yeoman.kontrakcjaRoot %>/texts/**/*.json",
          "<%= yeoman.kontrakcjaRoot %>/templates/javascript-langs.st"
        ],
        tasks: ["updateLocalization"]
      },
      apiExplorer: {
        files: ["<%= yeoman.app %>/api-explorer/**/*"],
        tasks: ["copy:apiExplorer"]
      }
    },

    webpack: {
      all: webpackConfig[0],
      signview: merge(
        webpackConfig[1],
        {
          devtool: false,
          plugins: [
            new webpack.optimize.UglifyJsPlugin({
              minimize: true,
              compress: {warnings: false}
            }),
            new webpack.DefinePlugin({
              "process.env": {"NODE_ENV": JSON.stringify("production")}
            })
          ]
        }
      ),
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
      options: {
        configFile: "karma.conf.js"
      },
      full: {
        autoWatch: false,
        browsers: ["PhantomJS"],
        reporters: ["teamcity", "coverage-istanbul"],
        singleRun: true,
        webpackMiddleware: {
          noInfo: true,
          stats: "errors-only"
        }
      },
      fast: {
        autoWatch: false,
        browsers: ["PhantomJS"],
        reporters: ["teamcity"],
        singleRun: true,
        webpackMiddleware: {
          noInfo: true,
          stats: "errors-only"
        }
      },
      dev: {
        singleRun: true
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
        // We can't just invoke `../shake.sh localization` because
        // Shake doesn't allow us to run two Shake processes in the
        // same working dir simultaneously.
        command: (newBuild ? "cabal new-build" : "cabal build") + " localization",
        options: {
          execOptions: {
            cwd: "<%= yeoman.kontrakcjaWorkspace %>"
          }
        }
      },
      generateLocalization: {
        command: (process.env.LOCALIZATION_BIN ||
                  (newBuild ? "cabal new-run localization" : "./dist/build/localization/localization")),
        options: {
          execOptions: {
            cwd: "<%= yeoman.kontrakcjaWorkspace %>"
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
              "enable-cookies/**/*",
              "css/fonts/*.ttf",
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
      },
      apiExplorer: {
        files: [
          {
            expand: true,
            dot: true,
            cwd: "<%= yeoman.app %>",
            dest: "<%= yeoman.dist %>",
            src: ["api-explorer/**/*"]
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
      dist: ["copy:dist", "copy:apiExplorer"],
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
          "<%= yeoman.app %>/../node_modules/es6-object-assign/dist/object-assign-auto.min.js",
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
          "<%= yeoman.app %>/../node_modules/es6-object-assign/dist/object-assign-auto.min.js",
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
      },
      signview: {
        src: [
          "<%= yeoman.app %>/../node_modules/es6-object-assign/dist/object-assign-auto.min.js",
          "<%= yeoman.app %>/bower_components/trackjs/tracker.js",
          "<%= yeoman.app %>/bower_components/jquery/dist/jquery.min.js",
          "<%= yeoman.app %>/bower_components/jquery-migrate/jquery-migrate.min.js",
          "<%= yeoman.app %>/bower_components/underscore/underscore-min.js",
          "<%= yeoman.app %>/bower_components/moment/min/moment.min.js",
          util.format("<%= yeoman.app %>/bower_components/moment/locale/{%s}.js", langFromTexts.join(",")),
          "<%= yeoman.app %>/bower_components/es6-promise/promise.min.js",
          "<%= yeoman.app %>/libs/base64.js",
          "<%= yeoman.app %>/libs/jquery.form.js",
          "<%= yeoman.app %>/libs/tinycolor-min.js",
          "<%= yeoman.app %>/libs/jstz.min.js",
          "<%= yeoman.app %>/js/global/cookie.js",
          "<%= yeoman.app %>/js/global/time.js",
          "<%= yeoman.app %>/js/global/shims.js",
          "<%= yeoman.app %>/js/global/errors.js",
          "<%= yeoman.app %>/js/global/csrf.js",
          "<%= yeoman.app %>/js/global/timezone.js"
        ],
        dest: "<%= yeoman.app %>/compiled/signview-vendor-" + generateVersionId() + ".js"
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
      "shell:generateLocalization"
    ]);
  });

  grunt.registerTask("updateLocalization", function (target) {
    return grunt.task.run([
      "shell:generateLocalization"
    ]);
  });

  grunt.registerTask("buildJs", function (target) {
    var tasks = [
      "uglify:prod",
      "uglify:signview",
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
      "uglify:signview",
      "concurrent:watch"
    ]);
  });

  grunt.registerTask("build:nix", function (target) {
    grunt.task.run([
      "clean:dist",
      "updateLocalization",
      "compileStyles",
      "buildJs",
      "cssmin:dist",
      "deploybuild:dist",
      "concurrent:dist",
      "shell:updateLastBuilt"
    ]);
  });

  grunt.registerTask("compileStyles", ["less", "autoprefixer"]);
  grunt.registerTask("server:dist", ["build"]);
  grunt.registerTask("test", ["eslint", "karma:full"]);
  grunt.registerTask("test:fast", ["karma:fast"]);
  grunt.registerTask("default", ["eslint", "build", "test"]);
};
