'use strict';

require('./app.scss');

var Elm = require('./Main.elm').Elm;

console.log('cdnBaseUrl:', window.cdnBaseUrl)

Elm.Main.init({
    node: document.getElementById('elm-mount')
  , flags:
      { cookie: document.cookie || ''
      , cdnBaseUrl : window.cdnBaseUrl || ''
      }
});
