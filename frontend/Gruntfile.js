var _ = require("underscore");
var webpackConfig = require("./webpack.config.js");

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
      allWatch: _.extend({
        watch: true,
        keepalive: true
      }, webpackConfig[0]),
      signviewWatch: _.extend({
        watch: true,
        keepalive: true
      }, webpackConfig[1])
    },

    jscs: {
      src: require("./jscs_manifest.json"),
      options: {
        preset: "yandex",
        validateQuoteMarks: "\"",
        disallowQuotedKeysInObjects: null,
        esprima: "esprima-fb"
      }
    },

    karma: {
      unitSingleRun: {
        singleRun: true,
        configFile: "karma.conf.js",
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
            "<%= yeoman.dist %>/*.js",
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
        command: "cabal build localization",
        options: {
          execOptions: {
            cwd: "<%= yeoman.kontrakcja %>"
          }
        }
      },
      generateLocalization: {
        command: "./dist/build/localization/localization",
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
              "fonts/**/*",
              "img/**/*",
              "newsletter/**/*",
              "pdf/**/*",
              "libs/tiny_mce/**/*",
              "localization/*.*.js",
              "libs/html5shiv.js",
              "bower_components/json3/**",
              "bower_components/es6-promise/**"
            ]
          }
        ]
      }
    },

    gjslint: {
      options: {
        flags: [
          "--disable 1,2,5,110,120,131,220",
          "--custom_jsdoc_tags jsx,note,description"
        ],
        reporter: {
          name: "console"
        }
      },
      all: {
        src: [
          "Gruntfile.js",
          "<%= yeoman.app %>/js/**/*.js",
          "<%= yeoman.app %>/compiled/**/*.js"
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
        browsers: ["ie >= 8", "chrome >= 31", "ff >= 38"]
      },
      compile: {
        src: "<%= yeoman.app %>/less/less-compiled.css"
      },
      signview: {
        src: "<%= yeoman.app %>/less/signview-less-compiled.css"
      }
    },

    uglify: {
      vendor: {
        options: {
          sourceMap: true
        },
        src: [
          "<%= yeoman.app %>/bower_components/jquery/jquery.js",
          "<%= yeoman.app %>/bower_components/underscore/underscore.js",
          "<%= yeoman.app %>/bower_components/backbone/backbone.js",
          "<%= yeoman.app %>/bower_components/react/react-with-addons.js",
          "<%= yeoman.app %>/bower_components/spin.js/spin.js",
          "<%= yeoman.app %>/bower_components/moment/moment.js",
          "<%= yeoman.app %>/libs/*.js",
          "<%= yeoman.app %>/js/global.js",
          "<%= yeoman.app %>/js/utils/time.js"
        ],
        dest: "<%= yeoman.app %>/compiled/vendor.js"
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
      "uglify",
      "webpack:all",
      "webpack:signview"
    ];

    return grunt.task.run(tasks);
  });

  grunt.registerTask("build", function (target) {
    var tasks = [
      "clean:dist",
      "compileGenerateLocalization",
      "concurrent:dist",
      "compileStyles",
      "buildJs",
      "cssmin:dist",
      "deploybuild:dist",
      "shell:updateLastBuilt"
    ];

    return grunt.task.run(tasks);
  });

  grunt.registerTask("server", function (target) {
    grunt.task.run([
      "clean:server",
      "compileStyles",
      "compileGenerateLocalization",
      "uglify",
      "concurrent:watch"
    ]);
  });

  grunt.registerTask("compileStyles", ["less", "autoprefixer"]);
  grunt.registerTask("server:dist", ["build"]);
  grunt.registerTask("test", ["uglify", "karma:unitSingleRun"]);
  grunt.registerTask("validateJs", ["gjslint"]);
  grunt.registerTask("default", ["gjslint", "build", "test"]);
};
