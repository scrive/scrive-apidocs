var path = require("path");
var webpack = require("webpack");
var _ = require("underscore");
var glob = require("glob");
var generateVersionId = require("./custom_grunt_tasks/utils/version_id_generator");
var merge = require("webpack-merge");

function mainResolver (filename) {
  return new webpack.ResolverPlugin(
    new webpack.ResolverPlugin.DirectoryDescriptionFilePlugin("./" + filename, ["main"])
  );
}

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
      loaders: [
        {
          test: /.jsx$/,
          loader: "babel",
          query: {
            presets: ["react", "es2015", "stage-2"]
          }
        },
        {
          test: /.less$/,
          loader: "less-interop"
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
      "base64": "Base64"
    },

    resolve: {
      extensions: ["", "min.js", ".js", ".jsx"],
      root: [path.join(__dirname, "./app/bower_components")]
    }
  }, obj);
}

var versionId = generateVersionId();

var signviewConfig = defaultConfig({
  name: "signview",
  output: {
    path: __dirname, filename: "./app/compiled/signview/[name]-" + versionId + ".js", sourceMapFilename: "[file].map"
  },
  entry: allEntryPoints("./app/scripts/entry/signview/*.jsx"),
  plugins: [
    mainResolver("package.json"),
    mainResolver("bower.json"),
    new webpack.optimize.UglifyJsPlugin({
      minimize: true,
      compress: {warnings: false}
    }),
    new webpack.optimize.DedupePlugin(),
    new webpack.DefinePlugin({
      "process.env": {"NODE_ENV": JSON.stringify("production")}
    })
  ]
});

var allConfig = defaultConfig({
  name: "all",
  entry: allEntryPoints("./app/scripts/entry/all/*.jsx"),
  output: {
    path: __dirname, filename: "./app/compiled/all/[name]-" + versionId + ".js", sourceMapFilename: "[file].map"
  },
  plugins: [mainResolver("package.json"), mainResolver("bower.json")]
});

module.exports = [allConfig, signviewConfig];
