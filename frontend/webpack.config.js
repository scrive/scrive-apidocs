var path = require("path");
var webpack = require("webpack");
var _ = require("underscore");
var glob = require("glob");

function bowerResolver() {
  return new webpack.ResolverPlugin(
    new webpack.ResolverPlugin.DirectoryDescriptionFilePlugin("./bower.json", ["main"])
  );
}

function commonsPlugin(output) {
  return new webpack.optimize.CommonsChunkPlugin({
    name: "common",
    filename: output,
    minChunks: 2
  });
}

var uglifyPlugin = new webpack.optimize.UglifyJsPlugin({minimize: true});

var context = path.join(__dirname, "/app");

function allEntryPoints(m) {
  return _.object(glob.sync(m).map(function (entry) {
    var name = path.basename(entry).split(".")[0];
    return [name, "./" + path.relative(context, entry)];
  }));
};

function defaultConfig(obj) {
  return _.extend({
    context: context,

    devtool: "source-map",

    module: {
      loaders: [
        {
          test: /.jsx$/,
          loader: "babel",
          query: {
            presets: ["react", "es2015"]
          }
        }
      ]
    },

    externals: {
      "jquery": "jQuery",
      "backbone": "Backbone",
      "underscore": "_",
      "react": "React",
      "react/addons": "React",
      "tinycolor": "tinycolor",
      "html2canvas": "html2canvas",
      "spin.js": "Spinner",
      "moment": "moment",
      "base64": "Base64"
    },

    resolve: {
      extensions: ["", "min.js", ".js", ".jsx"],
      root: [path.join(__dirname, "./app/bower_components")],
    },

  }, obj);
};

var signviewConfig = defaultConfig({
  name: "signview",
  output: {path: __dirname, filename: "./app/compiled/signview/[name].js", sourceMapFilename: "[file].map"},
  entry: allEntryPoints("./app/scripts/entry/signview/*.jsx"),
  plugins: [bowerResolver()]
});

var allConfig = defaultConfig({
  name: "all",
  entry: allEntryPoints("./app/scripts/entry/all/*.jsx"),
  output: {path: __dirname, filename: "./app/compiled/all/[name].js", sourceMapFilename: "[file].map"},
  plugins: [bowerResolver()]
});

module.exports = [allConfig, signviewConfig];
