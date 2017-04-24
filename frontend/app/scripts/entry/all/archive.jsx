var $ = require("jquery");

var ArchiveView = require("../../archive/archive.jsx");

$(function () {
  var $container = $("<div></div>");
  var view = React.render(
    React.createElement(ArchiveView, {
      companyAdmin: fromTemplate.forCompanyAdmin,
      idleDocumentTimeout: fromTemplate.idledoctimeout,
      month: fromTemplate.month,
      year: fromTemplate.year
    }),
    $container[0]
  );

  $(".archive").append($container);
});
