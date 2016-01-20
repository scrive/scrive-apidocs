var allTestFiles = [];
var TEST_REGEXP = /^\/base\/test/;
var TEST_DATA = /^\/base\/test\/data/;

var pathToModule = function(path) {
  return path.replace(/^\/base\//, '').replace(/\.js$/, '');
};

var notTestFiles = [
  "/base/test/env.js",
  "/base/test/setup.js",
  "/base/test/backend.js",
  "/base/test/util.js",
  "/base/test/image.js"
];

Object.keys(window.__karma__.files).forEach(function(file) {
  if (TEST_REGEXP.test(file) && notTestFiles.indexOf(file) === -1 && !TEST_DATA.test(file)) {
    allTestFiles.push(file);
  }
});

console.log(allTestFiles);

var realLoad = function () {
  var requirejsConfig = {
    waitSeconds: 8,
    baseUrl:'/base/compiled_jsx/',
    paths: {
      /**
       *  Libraries
       */
      jquery: '../bower_components/jquery/jquery.min',
      es5shim: '../bower_components/es5-shim/es5-shim.min',
      es5sham: '../bower_components/es5-shim/es5-sham.min',
      es6promise: '../bower_components/es6-promise/promise.min',
      React: '../bower_components/react/react-with-addons',
      Backbone: '../bower_components/backbone/backbone',
      Underscore: '../bower_components/underscore/underscore-min',
      Spinjs: '../bower_components/spin.js/spin',
      moment: '../bower_components/moment/min/moment-with-langs.min',
      tinycolor : '../libs/tinycolor-min',
      sinon: '../libs/sinon',
      backend: '../test/backend',
      util: '../test/util',
      image: '../test/image',
      /**
       *  Legacy code imports
       */
      'legacy_code': 'config/include_legacy_code',
      'legacy_code_for_signview': 'config/include_legacy_code_for_signview'
    },
    shim: {
      'Underscore': {
        exports: '_'
      },
      'Backbone': {
        deps: ['jquery', 'Underscore'],
        exports: 'Backbone'
      }
    },
    deps: ['es5shim', 'es5sham', 'es6promise', 'jquery', 'Underscore', 'Backbone'].concat(allTestFiles),
    callback: window.__karma__.start
  };

  require.config(requirejsConfig);
};

// hack to fix problems with jquery.hashchange
var mustLoadBefore = {
  waitSeconds: 8,
  baseUrl:'/base/compiled_jsx/',
  paths: {
    jquery: '../bower_components/jquery/jquery.min',
    es5shim: '../bower_components/es5-shim/es5-shim.min',
    es5sham: '../bower_components/es5-shim/es5-sham.min',
    es6promise: '../bower_components/es6-promise/promise.min',
    React: '../bower_components/react/react-with-addons',
    Backbone: '../bower_components/backbone/backbone',
    Underscore: '../bower_components/underscore/underscore-min',
    Spinjs: '../bower_components/spin.js/spin',
    moment: '../bower_components/moment/min/moment-with-langs.min',
    tinycolor : '../libs/tinycolor-min',
    html2canvas: '../libs/html2canvas',
    base64: '../libs/base64',

    /**
     *  Legacy code imports
     */
    'legacy_code': 'config/include_legacy_code',
    'legacy_code_for_signview': 'config/include_legacy_code_for_signview'
  },
  shim: {
    'jquery': {
      deps: ['es5shim'],
      exports: '$'
    },
    '../libs/jquery.browser.min': {
      deps: ['jquery']
    }
  },
  deps: ['es5shim', 'jquery', '../libs/jquery.browser.min'],
  callback: realLoad
};

require.config(mustLoadBefore);

