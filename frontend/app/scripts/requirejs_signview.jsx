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
    React: '../bower_components/react/react-with-addons',
    Backbone: '../bower_components/backbone/backbone',
    Underscore: '../bower_components/underscore/underscore-min',
    moment: '../bower_components/moment/min/moment-with-langs.min',
    html2canvas: '../libs/html2canvas',
    base64: '../libs/base64',
    /**
     *  Legacy code imports
     */
    'legacy_code': 'config/include_legacy_code_for_signview'
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
  include: [ 'pages/header', 'pages/footer', 'signview/header', 'signview/footer', 'signview/identify/identifyview', 'signview/signview']
};
