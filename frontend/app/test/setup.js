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
  "/base/test/localization.js"
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
      React: '../bower_components/react/react-with-addons',
      Backbone: '../bower_components/backbone/backbone',
      Underscore: '../bower_components/underscore/underscore-min',
      text: '../bower_components/requirejs-text/text',
      Q: '../bower_components/q/q',
      Spinjs: '../bower_components/spin.js/spin',
      eventie: '../bower_components/eventie',
      eventEmitter: '../bower_components/eventEmitter',
      imagesLoaded: '../bower_components/imagesloaded/imagesloaded',
      moment: '../bower_components/moment/min/moment-with-langs.min',
      should: '../bower_components/should/should',
      StateMachine: '../bower_components/javascript-state-machine/state-machine',
      tinyMCE: '../libs/tiny_mce/tinymce.min',
      tinyMCE_theme: '../libs/tiny_mce/themes/modern/theme.min',
      tinyMCE_noneeditable: '../libs/tiny_mce/plugins/noneditable/plugin.min',
      tinyMCE_paste: '../libs/tiny_mce/plugins/paste/plugin.min',
      tinycolor : '../libs/tinycolor-min',
      sinon: '../libs/sinon',
      backend: '../test/backend',
      util: '../test/util',
      /**
       *  Legacy code imports
       */
      'legacy_code': 'config/include_legacy_code'
    },
    shim: {
      'Underscore': {
        exports: '_'
      },
      'Backbone': {
        deps: ['jquery', 'Underscore'],
        exports: 'Backbone'
      },
      tinyMCE: {
        exports: 'tinyMCE',
        init: function () {
          this.tinyMCE.DOM.events.domLoaded = true;

          // needed to make tinymce load css from correct place
          this.tinyMCE.baseURL = window.location.origin + '/libs/tiny_mce';

          return this.tinyMCE;
        }
      },
      'tinyMCE_theme': {
        deps: ['tinyMCE']
      },
      'tinyMCE_noneeditable': {
        deps: ['tinyMCE']
      },
      'tinyMCE_paste': {
        deps: ['tinyMCE']

      },
    },
    deps: ['es5shim', 'es5sham', 'jquery', 'Underscore', 'Backbone'].concat(allTestFiles),
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
    React: '../bower_components/react/react-with-addons',
    Backbone: '../bower_components/backbone/backbone',
    Underscore: '../bower_components/underscore/underscore-min',
    text: '../bower_components/requirejs-text/text',
    Q: '../bower_components/q/q',
    Spinjs: '../bower_components/spin.js/spin',
    eventie: '../bower_components/eventie',
    eventEmitter: '../bower_components/eventEmitter',
    imagesLoaded: '../bower_components/imagesloaded/imagesloaded',
    moment: '../bower_components/moment/min/moment-with-langs.min',
    should: '../bower_components/should/should',
    StateMachine: '../bower_components/javascript-state-machine/state-machine',
    tinyMCE: '../libs/tiny_mce/tinymce.min',
    tinyMCE_theme: '../libs/tiny_mce/themes/modern/theme.min',
    tinyMCE_noneeditable: '../libs/tiny_mce/plugins/noneditable/plugin.min',
    tinyMCE_paste: '../libs/tiny_mce/plugins/paste/plugin.min',
    tinycolor : '../libs/tinycolor-min',
    html2canvas: '../libs/html2canvas',
    /**
     *  Legacy code imports
     */
    'legacy_code': 'config/include_legacy_code'
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

