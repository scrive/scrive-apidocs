var path = require("path");
var webpack = require("webpack");
var generateVersionId = require("./custom_grunt_tasks/utils/version_id_generator");

module.exports = function(config) {
  var newConfig = {
    basePath: ".",

    frameworks: ["mocha", "sinon-chai"],

    files: [
      "./app/localization/*.en.js",
      "./app/bower_components/jquery/dist/jquery.js",
      "./app/bower_components/moment/moment.js",
      "./app/bower_components/underscore/underscore.js",
      "./app/libs/*.js",
      "./app/js/global/*.js",
      "./app/test/env.js",
      "./app/test/entry.js"
    ],

    preprocessors: {
      "./app/test/entry.js": ["webpack", "sourcemap"]
    },

    reporters: ["mocha"],

    port: 9876,

    colors: true,

    logLevel: config.LOG_INFO,

    autoWatch: false,

    browsers: [
      'ChromeHeadlessNoSandbox'
    ],

    customLaunchers: {
      ChromeHeadlessNoSandbox: {
        base: 'ChromeHeadless',
        flags: ['--no-sandbox']
      }
    },

    singleRun: false,

    webpack: {
      devtool: "inline-source-map",

      module: {
        rules: [
          {
            test: /\.jsx$/,
            loader: "babel-loader"
          },
          {
            test: /\.less$/,
            use: [
              {
                loader: "css-loader"
              },
              {
                loader: "less-loader"
              }
            ]
          },
          {
            test: /\.svg$/,
            use: [
              {
                loader: "babel-loader"
              },
              {
                loader: "svg-react-loader"
              }
            ]
          },
          {
            test: /sinon\.js$/,
            loader: "imports-loader?define=>false,require=>false"
          },
        ],
        noParse: [
          /html2canvas/
        ],
      },
      mode: "development",

      externals: {
        "jquery": "jQuery",
        "underscore": "_",
        "moment": "moment",
        "base64": "Base64"
      },

      resolve: {
        extensions: ["min.js", ".js", ".jsx"],
        alias: {
          "backbone": "backbone/backbone.js",
          "jquery.documentsize": "jquery.documentsize/dist/jquery.documentsize.js",
          "tinycolor": path.resolve(__dirname) + "/app/libs/tinycolor-min.js",
          "react": "react/react-with-addons",
          "react-dom": "react/react-with-addons",
          "spin.js": "spin.js/spin.js",
          "_": "underscore"
        },
        modules: [
          path.join(__dirname, "./app/bower_components"),
          "node_modules"
        ]
      }
    },
    webpackMiddleware: {
      stats: "normal"
    }
  };

  if (config.reporters.indexOf("coverage-istanbul") != -1) {
    newConfig.coverageIstanbulReporter = {
      reports: ["html"],
      dir: "./coverage/",
      fixWebpackSourcePaths: true
    };

    newConfig.webpack.module.rules.unshift({
      test: /\.jsx$/,
      loader: "istanbul-instrumenter-loader"
    });
  }

  config.set(newConfig);
};
