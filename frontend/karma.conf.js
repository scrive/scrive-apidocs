module.exports = function(config) {
  config.set({
    basePath: "./app/",

    frameworks: ["requirejs", "mocha", "chai"],

    files: [
      // Include EN localisation for tests but all others for JSCS pre-processor
      "./localization/*.en.js",
      {pattern: "./localization/*.*.js", included: false},
      "./test/env.js",
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
      "./compiled_jsx/**/*.js": ["coverage"],
      "./localization/*.*.js" : ["jscs"]
    },

    jscsPreprocessor: {
      rules: {
        // Single rule to check that localisation is valid JavaScript
        "esnext": true
      }
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
