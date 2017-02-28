var path = require("path");
var webpack = require("webpack");
var _ = require("underscore");
var glob = require("glob");
var generateVersionId = require("./custom_grunt_tasks/utils/version_id_generator");
var merge = require("webpack-merge");

var context = path.join(__dirname, "/app");

function allEntryPoints (m) {
  return _.object(glob.sync(m).map(function (entry) {
    var name = path.basename(entry).split(".")[0];
    return [name, "./" + path.relative(context, entry)];
  }));
}

function defaultConfig (obj) {
  return merge({
    context: context,

    devtool: "source-map",

    module: {
      rules: [
        {
          test: /\.jsx$/,
          loader: "babel-loader",
          options: {
            presets: ["react", "es2015", "stage-2"]
          }
        },
        {
          test: /\.less$/,
          use: [
            {
              loader: "less-interop-loader"
            }
          ]
        },
        {
          test: /\.svg$/,
          use: [
            {
              loader: "babel-loader",
              options: {
                presets: ["react", "es2015", "stage-2"]
              }
            },
            {
              loader: "svg-react-loader"
            }
          ]
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
      "react-dom": "React",
      "react/addons": "React",
      "tinycolor": "tinycolor",
      "moment": "moment",
      "base64": "Base64"
    },

    resolve: {
      extensions: ["min.js", ".js", ".jsx"],
      alias: {
        "spin.js": "spin.js/spin.js",
        "jquery.documentsize": "jquery.documentsize/dist/jquery.documentsize.js"
      },
      modules: [
        path.join(__dirname, "./app/bower_components"),
        "node_modules"
      ]
    }
  }, obj);
}

var versionId = generateVersionId();

var signviewConfig = defaultConfig({
  name: "signview",
  output: {
    path: __dirname,
    filename: "./app/compiled/signview/[name]-" + versionId + ".js",
    sourceMapFilename: "[file].map"
  },
  entry: allEntryPoints("./app/scripts/entry/signview/*.jsx"),
  plugins: [
    new webpack.optimize.UglifyJsPlugin({
      minimize: true,
      compress: {warnings: false}
    }),
    new webpack.DefinePlugin({
      "process.env": {"NODE_ENV": JSON.stringify("production")}
    })
  ]
});

var allConfig = defaultConfig({
  name: "all",
  entry: allEntryPoints("./app/scripts/entry/all/*.jsx"),
  output: {
    path: __dirname,
    filename: "./app/compiled/all/[name]-" + versionId + ".js",
    sourceMapFilename: "[file].map"
  }
});

module.exports = [allConfig, signviewConfig];
