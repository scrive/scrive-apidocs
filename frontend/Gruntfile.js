var JSX_BIN = __dirname + "/node_modules/react-tools/bin/jsx ";

module.exports = function (grunt) {
  require("load-grunt-tasks")(grunt);
  require("time-grunt")(grunt);
  require("./custom_grunt_tasks/deploybuild")(grunt);

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

    requirejs: {
      options: {
        baseUrl: "<%= yeoman.app %>/compiled_jsx/",
        mainConfigFile: "<%= yeoman.app %>/compiled_jsx/requirejs_config.js",
        out: "<%= yeoman.dist %>/optimized-system.js",
        include: ["../bower_components/requirejs/require.js"],
        preserveLicenseComments: false
      },
      withSourceMap: {
        options: {
          optimize: "uglify2",
          generateSourceMaps: true
        }
      },
      noSourceMap: {
        options: {
          optimize: "none",
          generateSourceMaps: false
        }
      }
    },

    clean: {
      dist: {
        files: [
          {
            dot: true,
            src: [
              ".tmp",
              "<%= yeoman.app %>/compiled_jsx/",
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
              "<%= yeoman.app %>/compiled_jsx/"
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
      compileJsx: {
        command: JSX_BIN + " -x jsx <%= yeoman.app %>/scripts/ <%= yeoman.app %>/compiled_jsx/"
      },
      watchJsx: {
        command: JSX_BIN + " -x jsx -w <%= yeoman.app %>/scripts/ <%= yeoman.app %>/compiled_jsx/"
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
              "css/fonts/*",
              "css/assets/*",
              "fonts/**/*",
              "img/**/*",
              "newsletter/**/*",
              "pdf/**/*",
              "libs/tiny_mce/**/*",
              "localization/*.*.js",
              "libs/html5shiv.js",
              "bower_components/json3/**",
              "bower_components/es5-shim/**"
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
          "<%= yeoman.app %>/compiled_jsx/**/*.js"
        ]
      }
    },

    cssmin: {
      options: {
        report: true
      },
      dist: {
        files: {
          "<%= yeoman.dist %>/all-styling-minified.css": [
            "<%= yeoman.app %>/css/*.css",
            "<%= yeoman.app %>/less/less-compiled.css"
          ]
        }
      },
      dev: {
        files: {
          "<%= yeoman.app %>/less/regular-css-compiled.css": [
            "<%= yeoman.app %>/css/*.css"
          ]
        }
      }
    },

    concurrent: {
      dist: ["copy:dist"],
      watch: {
        tasks: ["watch", "shell:watchJsx"],
        options: {logConcurrentOutput: true}
      }
    },

    autoprefixer: {
      options: {
        browsers: ["ie >= 8", "chrome >= 31", "ff >= 38"]
      },
      compile: {
        src: "<%= yeoman.app %>/less/less-compiled.css"
      }
    },

    less: {
      compile: {
        options: {
          paths: ["<%= yeoman.app %>/less"],
          sourceMap: true,
          sourceMapFilename: "<%= yeoman.app %>/less/less-compiled.css.map",
          sourceMapURL: "/less/less-compiled.css.map"
        },
        files: {
          "<%= yeoman.app %>/less/less-compiled.css": "<%= yeoman.app %>/less/index.less"
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

  grunt.registerTask("build", function (target) {
    var tasks = [
      "clean:dist",
      "compileGenerateLocalization",
      "concurrent:dist",
      "shell:compileJsx"
    ];

    if (target === "sourcemap") {
      tasks.push("requirejs:withSourceMap");
    } else {
      tasks.push("requirejs:noSourceMap");
    }

    tasks = tasks.concat([
      "compileStyles",
      "cssmin:dist",
      "deploybuild:dist",
      "shell:updateLastBuilt"
    ]);

    return grunt.task.run(tasks);
  });

  grunt.registerTask("server", function (target) {
    grunt.task.run([
      "clean:server",
      "compileStyles",
      "cssmin:dev",
      "compileGenerateLocalization",
      "shell:compileJsx",
      "concurrent:watch"
    ]);
  });

  grunt.registerTask("compileStyles", ["less:compile", "autoprefixer:compile"]);
  grunt.registerTask("server:dist", ["build"]);
  grunt.registerTask("test", ["karma:unitSingleRun"]);
  grunt.registerTask("validateJs", ["gjslint"]);
  grunt.registerTask("default", ["gjslint", "build", "test"]);
};
