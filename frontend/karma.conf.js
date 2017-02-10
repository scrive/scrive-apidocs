var path = require("path");
var webpack = require("webpack");
var generateVersionId = require("./custom_grunt_tasks/utils/version_id_generator");

function bowerResolver() {
  return new webpack.ResolverPlugin(
    new webpack.ResolverPlugin.DirectoryDescriptionFilePlugin("./bower.json", ["main"])
  );
}

module.exports = function(config) {
  var newConfig = {
    basePath: "./app/",

    frameworks: ["mocha", "sinon-chai"],

    files: [
      "./localization/*.en.js",
      "./bower_components/jquery/dist/jquery.js",
      "./bower_components/moment/moment.js",
      "./bower_components/underscore/underscore.js",
      "./libs/*.js",
      "./js/global/*.js",
      "./test/env.js",
      "./test/entry.js"
    ],

    preprocessors: {
      "./test/entry.js": ["webpack", "sourcemap"]
    },

    reporters: ["progress"],

    port: 9876,

    colors: true,

    logLevel: config.LOG_INFO,

    autoWatch: false,

    browsers: ["ChromeCanary"],

    singleRun: false,

    webpack: {
      context: "./app",

      devtool: "inline-source-map",

      module: {
        loaders: [
          {
            test: /\.jsx$/,
            loader: "babel",
            query: {
              presets: ["react", "es2015", "stage-2"]
            }
          },
          {
            test: /\.less$/,
            loader: "less-interop",
          },
          {
            test: /\.svg$/,
            loader: "babel?presets[]=stage-2,presets[]=es2015,presets[]=react!svg-react"
          },
          {
            test: /sinon\.js$/,
            loader: "imports?define=>false,require=>false"
          }
        ],
        noParse: [
          /html2canvas/
        ]
      },

      externals: {
        "jquery": "jQuery",
        "underscore": "_",
        "moment": "moment",
        "base64": "Base64"
      },

      resolve: {
        extensions: ["", "min.js", ".js", ".jsx"],
        root: [path.join(__dirname, "./app/bower_components")],
        alias: {
          "tinycolor": path.resolve(__dirname) + "/app/libs/tinycolor-min.js",
          "react": "react/react-with-addons",
          "react-dom": "react/react-with-addons",
          "_": "underscore"
        }
      },

      plugins: [bowerResolver()],
    },
    webpackMiddleware: {
      stats: "normal"
    }
  };

  if (config.reporters.indexOf("coverage") != -1) {
    newConfig.coverageReporter = {
      type: "html",
      dir: "../coverage/"
    };

    newConfig.webpack.isparta = {
      embedSource: true,
      noAutoWrap: true,
      babel: {
        presets: ["react", "es2015"]
      }
    };

    newConfig.webpack.module.preLoaders = [
      {
        test: /\.jsx$/,
        loader: "isparta"
      },
      newConfig.webpack.module.loaders[0]
    ];
    newConfig.webpack.module.loaders.shift();
  }

  config.set(newConfig);
};
