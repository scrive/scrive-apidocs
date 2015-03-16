/**
 *  @description
 *  Setup RequireJS library paths and shims
 *
 *  @note
 *  TODO(jens): Tinymce and plugins, should be set as a 'requirejs bundles' property
 *              so we don't have to load them all separately, whenever we use them.
 *              Didn't get 'requirejs bundles' to work when I tried.
 */

var require = {
  waitSeconds: 8,
  baseUrl:'/compiled_jsx/',
  paths: {
    /**
     *  Libraries
     */
    jquery: '../bower_components/jquery/jquery.min',
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
    StateMachine: '../bower_components/javascript-state-machine/state-machine',
    tinycolor : '../libs/tinycolor-min',
    html2canvas: '../libs/html2canvas',
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
    }
  },
  deps: ['jquery', 'Underscore', 'Backbone'],
  // All scripts that are not used by other components, but refered from string templates should be listed here
  include: ['signview/header', 'signview/footer', 'login/login', 'pages/header', 'pages/footer', 'lists/list', 'padlist/padlist', 'createfromtemplate/createfromtemplate', 'postsignview/archive_views', 'postsignview/psv_document_model', 'to-start/to-start', 'to-start/templatelist']
};
