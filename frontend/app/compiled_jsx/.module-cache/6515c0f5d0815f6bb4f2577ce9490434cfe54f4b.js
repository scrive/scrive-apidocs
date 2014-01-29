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
  waitSeconds: 0,
  baseUrl:'/compiled_jsx/',
  paths: {
    /**
     *  Libraries
     */
    jquery: '../bower_components/jquery/jquery.min',
    React: '../bower_components/react/react-with-addons.min',
    Backbone: '../bower_components/backbone/backbone',
    Underscore: '../bower_components/underscore/underscore-min',
    text: '../bower_components/requirejs-text/text',
    Q: '../bower_components/q/q',
    Spinjs: '../bower_components/spin.js/spin',
    eventie: '../bower_components/eventie',
    eventEmitter: '../bower_components/EventEmitter',
    imagesLoaded: '../bower_components/imagesloaded/imagesloaded',
    tinyMCE: '../libs/tiny_mce/tinymce.min',
    tinyMCE_theme: '../libs/tiny_mce/themes/modern/theme.min',
    tinyMCE_noneeditable: '../libs/tiny_mce/plugins/noneditable/plugin.min',
    tinyMCE_paste: '../libs/tiny_mce/plugins/paste/plugin.min',
    /**
     *  Legacy code imports
     */
    'utils/cookie': '../js/utils/cookie',
    legacy_code: 'config/include_legacy_code'
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
  deps: ['jquery', 'Underscore', 'Backbone']
};
