safeReady(function() {
  $("#passwordInfo").hide();

  $("#tosCBox").change(function() {
    if ($("#tosCBox").attr("checked")) {
      $("#passwordInfo").show();
    } else {
      $("#passwordInfo").hide();
    }
  });
});
