function hideEverything() {
    $("#accountTypeChoice").hide();
    $("#companyInfo").hide();
    $("#passwordInfo").hide();
}

function displayAccoutTypeChoice() {
    if ($("#tosCBox").attr("checked")) {
        $("#accountTypeChoice").show();
        displayAdditionalInfo();
    } else
        hideEverything();
}

function displayAdditionalInfo() {
    if ($("#tosCBox").attr("checked")) {
        if ($("#accountTypeCompany").attr("checked")) {
            $("#companyInfo").show();
            $("#passwordInfo").show();
        } else if ($("#accountTypePrivate").attr("checked")) {
            $("#companyInfo").hide();
            $("#passwordInfo").show();
        }
    }
}

safeReady(function() {
    hideEverything();
    displayAccoutTypeChoice();
    displayAdditionalInfo();

    $("#tosCBox").change(displayAccoutTypeChoice);
    $("input[name=accounttype]").change(displayAdditionalInfo);
});
