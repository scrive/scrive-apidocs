'use strict';

var LIVERELOAD_PORT = 35729;
var lrSnippet = require('connect-livereload')({ port: LIVERELOAD_PORT });
var mountFolder = function(connect, dir) {
  return connect.static(require('path').resolve(dir));
};

module.exports = function(grunt) {
  require('load-grunt-tasks')(grunt);
  require('time-grunt')(grunt);
  grunt.loadNpmTasks('grunt-shell-spawn');
  require('./custom_grunt_tasks/deploybuild')(grunt);

  // configurable paths
  var yeomanConfig = {
    app: 'app',
    dist: 'dist'
  };

  try {
    yeomanConfig.app = require('./bower.json').appPath || yeomanConfig.app;
  } catch (e) {
  }

  grunt.initConfig({
    yeoman: yeomanConfig,
    watch: {
      less: {
        files: ["<%= yeoman.app %>/less/**/*.less", "<%= yeoman.app %>/scripts/temporary_less_directory/**/*.less"],
        tasks: ["less:compile"]
      },
      livereload: {
        options: {
          livereload: LIVERELOAD_PORT
        },
        files: [
          '<%= yeoman.app %>/less/*.css',
          '.tmp/styles/{,*/}*.css',
          '{.tmp,<%= yeoman.app %>}/compiled_jsx/**/*.js',
          '<%= yeoman.app %>/js/**/*.js',
          '<%= yeoman.app %>/images/{,*/}*.{png,jpg,jpeg,gif,webp,svg}'
        ]
      }
    },
    connect: {
      proxyserver: {
        // Proxy connections to either kontraktcja or angularjs
        options: {
          debug: true,
          hostname: '*',
          port: 9000,
          middleware: function(connect, options) {
            // Configure grunt-connect-proxy

            var config = [ // Serve static files.
              connect.static(options.base),
              // Make empty directories browsable.
              connect.directory(options.base)
            ];
            var proxy = require('grunt-connect-proxy/lib/utils').proxyRequest;
            config.unshift(proxy);
            return config;
          }
        },
        proxies: [
          // Serve most of the static files with grunt (since it's faster atleast on mac)
          {
            context: ['/js', '/libs', '/compiled_jsx', '/img', '/css', '/bower_components'],
            host: '127.0.0.1',
            port: 9001,
            https: false
          },
          // this gives access to kontraktcja under root
          {
            context: ['/'],
            host: '127.0.0.1',
            port: 8000,
            https: false
          }
        ]
      },
      livereload: {
        // this is the main server for serving static files
        options: {
          // Change this to '0.0.0.0' to access the server from outside.
          hostname: '*',
          port: 9001,
          middleware: function(connect) {
            return [
              lrSnippet,
              mountFolder(connect, '.tmp'),
              mountFolder(connect, yeomanConfig.app)
            ];
          }
        }
      },
      test: {
        options: {
          port: 9005,
          middleware: function(connect) {
            return [
              mountFolder(connect, '.tmp'),
              mountFolder(connect, 'test')
            ];
          }
        }
      },
      dist: {
        options: {
          port: 9001,
          middleware: function(connect) {
            return [
              mountFolder(connect, yeomanConfig.dist)
            ];
          }
        }
      }
    },
    karma: {
      unitSingleRun: {
        singleRun: true,
        configFile: 'test/karma-unit.conf.js',
        autoWatch: false // If true: run tests automatically when a file changes. Should be false on CI server.
      },
      unitWatch: {
        singleRun: false,
        configFile: 'test/karma-unit.conf.js',
        autoWatch: true
      }
    },
    open: {
      server: {
        url: 'http://localhost:<%= connect.proxyserver.options.port %>/r/'
      }
    },
    requirejs: {
      options: {
        baseUrl: '<%= yeoman.app %>/compiled_jsx/',
        mainConfigFile: '<%= yeoman.app %>/compiled_jsx/requirejs_config.js',
        out: '<%= yeoman.dist %>/optimized-system.js',
        include: ['../bower_components/requirejs/require.js'],
        name: 'app',
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
              '.tmp',
              '<%= yeoman.app %>/compiled_jsx/',
              '<%= yeoman.dist %>/*',
              '!<%= yeoman.dist %>/.git*'
            ]
          }
        ]
      },
      server: {
        files: [
          {
            dot: true,
            src: [
              '.tmp',
              '<%= yeoman.app %>/compiled_jsx/'
            ]
          }
        ]
      }
    },
    jshint: {
      options: {
        // this .jshintrc contains jshint options, which may overwrite options we set here
        jshintrc: '.jshintrc',
        ignores: [
        ]
      },
      all: [
        '<%= yeoman.app %>/compiled_jsx/**/*.js'
      ]
    },
    deploybuild: {
      dist: {
        files: {
          src: [
            '<%= yeoman.dist %>/*.js',
            '<%= yeoman.dist %>/*.css'
          ]
        },
        htmlFile: '<%= yeoman.dist %>/index.html'
      }
    },
    // Disabled until we get a better image structure (all images under one directory)
    imagemin: {
      dist: {
        files: [
          {
            expand: true,
            cwd: '<%= yeoman.app %>/img',
            src: '{,*/}*.{png,jpg,jpeg}',
            dest: '<%= yeoman.dist %>/img'
          }
        ]
      }
    },
    shell: {
      options: {
        stdout: true,
        stderr: true,
        failOnError: true
      },
      tar: {
        command: [
          'echo "Version $VER" > <%= yeoman.dist %>/VERSION',
          'tar -czf growth-$VER.tar.gz ./dist/'
        ].join(';')
      },
      updateLastBuilt: {
        command: 'echo "$(date +%s)" > <%= yeoman.dist %>/LAST_BUILT'
      },
      // TODO(jens): Move this compilation to a grunt plugin
      // The existing grunt-react plugin doesnt support -w parameter as far as I can see
      compileJsxWatch: {
        command: 'jsx -x jsx -w <%= yeoman.app %>/scripts/ <%= yeoman.app %>/compiled_jsx/',
        options: {
          async: true
        }
      },
      compileJsx: {
        command: 'jsx -x jsx <%= yeoman.app %>/scripts/ <%= yeoman.app %>/compiled_jsx/'
      }
    },
    copy: {
      dist: {
        files: [
          {
            expand: true,
            dot: true,
            cwd: '<%= yeoman.app %>',
            dest: '<%= yeoman.dist %>',
            src: [
              './*.{html,xml,jpg,png,ico,txt}',
              'api-demo/**/*',
              'enable-cookies/**/*',
              'css/fonts/*',
              'css/tinymce.css',
              'fonts/**/*',
              'img/**/*',
              'newsletter/**/*',
              'pdf/**/*',
              'libs/tiny_mce/**/*',

              // Shims that need to be loaded separately
              'libs/html5shiv.js',

              'bower_components/json3/**',
              'bower_components/es5-shim/**'
            ]
          },
        ]
      },
    },
    gjslint: {
      options: {
        flags: [
          '--disable 1,2,5,110,120,131,220', // TODO(jens): Fix so these pass
          '--custom_jsdoc_tags jsx,note,description'
        ],
        reporter: {
          name: 'console'
        }
      },
      all: {
        src: [
          '<%= yeoman.app %>/js/**/*.js',
          '<%= yeoman.app %>/compiled_jsx/**/*.js'
        ]
      }
    },
    cssmin: {
      options: {
        report: true
      },
      dist: {
        files: {
          '<%= yeoman.dist %>/all-styling-minified.css': [
            '<%= yeoman.app %>/css/*.css',
            '<%= yeoman.app %>/less/less-compiled.css',
            '!<%= yeoman.app %>/css/tinymce.css'
          ]
        }
      },
      dev: {
        files: {
          '<%= yeoman.app %>/less/regular-css-compiled.css': [
            '<%= yeoman.app %>/css/*.css',
            '!<%= yeoman.app %>/css/tinymce.css'
          ]
        }
      }
    },
    concurrent: {
      dist: [
        'copy:dist',
      ]
    },
    less: {
      compile: {
        options: {
          paths: ['<%= yeoman.app %>/less'],
          sourceMap: true,
          sourceMapFilename: '<%= yeoman.app %>/less/less-compiled.css.map',
          sourceMapURL: '/less/less-compiled.css.map'
        },
        files: {
          "<%= yeoman.app %>/less/less-compiled.css": "<%= yeoman.app %>/less/index.less"
        }
      }
    }
  });


  /**
   *  User facing Grunt Tasks
   */

  grunt.registerTask('compileJsxWatch', function(target) {
    return grunt.task.run([
      // compiled_jsx directory need to exist for it to start watch, i.e. run compileJsx first
      'shell:compileJsx',
      'shell:compileJsxWatch'
    ]);
  });

  /**
   *  Get a production looking enviroment, i.e.
   *  serve files compiled files from dist/
   *
   *  @note
   *  Expects 'grunt build' to have been run.
   *
   */
  grunt.registerTask('server:dist_run', function(target) {
    grunt.task.run([
      'configureProxies:proxyserver',
      'connect:proxyserver',
      'connect:dist:keepalive'
    ]);
  });

  /**
   *  Get a production looking enviroment, i.e.
   *  serve files compiled files from dist/
   *  build task is run as a dependency of this task
   */
  grunt.registerTask('server:dist', [
    'build',
    'server:dist_run'
  ]);


  /**
   *  Serve and compile static files, while developing.
   */
  grunt.registerTask('server', function(target) {
    grunt.task.run([
      'clean:server',
      'less:compile',
      'cssmin:dev',
      'compileJsxWatch',
      'configureProxies:proxyserver',
      'connect:proxyserver',
      'connect:livereload',
      'watch'
    ]);
  });


  /**
   *  Commands to run tests, mainly for CI-server usage
   *
   *  All tests expects 'grunt build' to have been run
   */
  grunt.registerTask('test', function(target) {
    if (target === 'unit') {
      return grunt.task.run([
        'unittests:singleRun'
      ]);
    } else {
      return grunt.task.run([
        'configureProxies:proxyserver',
        'connect:proxyserver',
        'connect:dist',
        'connect:test',
        'karma:unitSingleRun'
      ]);
    }
  });


  /**
   *  Unit tests (with karma + jasmine)
   */
  grunt.registerTask('unittests', [
    'connect:test',
    'karma:unitWatch'
  ]);

  grunt.registerTask('unittests:singleRun', [
    'connect:test',
    'karma:unitSingleRun'
  ]);

  /**
   *  Build files for production
   */
  grunt.registerTask('build', function(target) {
    var tasks = [
      'clean:dist',
      'concurrent:dist',
      'shell:compileJsx'
    ];

    if (target === 'sourcemap') {
      tasks.push('requirejs:withSourceMap');
    } else {
      tasks.push('requirejs:noSourceMap');
    }

    tasks = tasks.concat([
      'less:compile',
      'cssmin:dist',
      'deploybuild:dist',
      'shell:updateLastBuilt'
    ]);

    return grunt.task.run(tasks);
  });

  grunt.registerTask('package', [
    'build',
    'shell:tar'
  ]);

  grunt.registerTask('validateJs', [
    'gjslint'
  ]);

  grunt.registerTask('default', [
    'gjslint',
    'build',
    'test'
  ]);
};
