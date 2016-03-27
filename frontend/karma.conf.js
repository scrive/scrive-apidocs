var path = require("path");
var webpack = require("webpack");
var generateVersionId = require("./custom_grunt_tasks/utils/version_id_generator");

function bowerResolver() {
  return new webpack.ResolverPlugin(
    new webpack.ResolverPlugin.DirectoryDescriptionFilePlugin("./bower.json", ["main"])
  );
}

module.exports = function(config) {
  config.set({
    basePath: "./app/",

    frameworks: ["mocha", "sinon-chai"],

    files: [
      "./localization/*.en.js",
      "./compiled/vendor-" + generateVersionId() + ".js",
      "./test/env.js",
      "./test/entry.js"
    ],

    preprocessors: {
      "./test/entry.js": ["webpack", "sourcemap"]
    },

    reporters: ["progress", "coverage"],

    port: 9876,

    colors: true,

    logLevel: config.LOG_INFO,

    autoWatch: false,

    browsers: ["PhantomJS"],

    singleRun: false,

    webpack: {
      context: "./app",

      devtool: "inline-source-map",

      isparta: {
        embedSource: true,
        noAutoWrap: true,
        babel: {
          presets: ["react", "es2015"]
        }
      },

      module: {
        preLoaders: [
          {
            test: /.jsx$/,
            loader: "babel",
            query: {
              presets: ["react", "es2015", "stage-2"]
            }
          },
          {
            test: /\.jsx$/,
            loader: "isparta"
          }
        ],
        loaders: [
          {
            test: /.less$/,
            loader: "less-interop",
          }
        ],
        noParse: [
          /html2canvas/
        ]
      },

      externals: {
        "jquery": "jQuery",
        "backbone": "Backbone",
        "underscore": "_",
        "react": "React",
        "react/addons": "React",
        "tinycolor": "tinycolor",
        "moment": "moment",
        "sinon": "sinon",
        "base64": "Base64"
      },

      resolve: {
        extensions: ["", "min.js", ".js", ".jsx"],
        root: [path.join(__dirname, "./app/bower_components")],
      },

      plugins: [bowerResolver()]
    },

    coverageReporter: {
      type : "html",
      dir : "../coverage/"
    }
  });
};
