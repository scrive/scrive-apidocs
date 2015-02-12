module.exports = function(config) {
  config.set({
    basePath: "./app/",

    frameworks: ["requirejs", "mocha", "chai"],

    files: [
      "./test/env.js",
      "./test/localization.js",
      "./test/setup.js",
      {pattern: "./bower_components/**/*.js", included: false},
      {pattern: "./libs/**/*.js", included: false},
      {pattern: "./js/**/*.js", included: false},
      {pattern: "./compiled_jsx/**/*.js", included: false},
      {pattern: "./test/**/*.js", included: false}
    ],

    exclude: [
    ],

    preprocessors: {
      "./compiled_jsx/**/*.js": ["coverage"]
    },

    reporters: ["progress", "coverage"],

    port: 9876,

    colors: true,

    logLevel: config.LOG_INFO,

    autoWatch: false,

    browsers: ["PhantomJS"],

    singleRun: false,

    coverageReporter: {
      type : "html",
      dir : "../coverage/"
    }
  });
};
