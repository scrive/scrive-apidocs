//var context = require.context(".", true, /^\.\/test\/.+\.js?$/);
var context = require.context(".", true, /\.\/.+\/.+\.js$/);
context.keys().forEach(context);
module.exports = context;
