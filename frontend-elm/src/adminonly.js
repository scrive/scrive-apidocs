'use strict';

require('./app.scss');

var AdminOnly = require('./Entry/AdminOnly.elm').Elm.Entry.AdminOnly;

console.log('cdnBaseUrl:', window.cdnBaseUrl)

AdminOnly.init({
    node: document.getElementById('elm-mount')
  , flags:
      { cookie: document.cookie || ''
      , cdnBaseUrl : window.cdnBaseUrl || ''
      }
});
