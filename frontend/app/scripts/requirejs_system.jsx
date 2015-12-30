/**
 *  Require conf for everything except sign view
 */

var require = {
  waitSeconds: 8,
  baseUrl:'/compiled_jsx/',
  paths: {
    /**
     *  Libraries
     */
    jquery: '../bower_components/jquery/jquery.min',
    React: '../bower_components/react/react-with-addons.min',
    Backbone: '../bower_components/backbone/backbone-min',
    Underscore: '../bower_components/underscore/underscore-min',
    Spinjs: '../bower_components/spin.js/spin',
    moment: '../bower_components/moment/min/moment-with-langs.min',
    tinycolor : '../libs/tinycolor-min',
    html2canvas: '../libs/html2canvas',
    base64: '../libs/base64',
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
  include: ['login/login', 'pages/header', 'pages/special/header', 'pages/footer', 'pages/special/footer', 'lists/list', 'padlist/padlist', 'createfromtemplate/createfromtemplate', 'to-start/to-start', 'to-start/templatelist']
};
