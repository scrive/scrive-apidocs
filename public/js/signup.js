safeReady(function() {
  $("#signupInfo").hide();

  $("#tosCBox").change(function() {
    if ($("#tosCBox").attr("checked")) {
      $("#signupInfo").show();
      $("#signupInfo input")[0].focus();
    } else {
      $("#signupInfo").hide();
    }
  });
});
