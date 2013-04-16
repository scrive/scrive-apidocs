/* Helper methods to create a popup where user information is filled in. */

(function(window) {
var form = [
    "<div class='account-body'>",
        "<p>Create new user account</p>",
        "<table>",
            "<tr>",
                "<td>First name:</td>",
                "<td><input id='fstname' type='text' name='fstname' autocomplete='off' /></td>",
            "</tr>",
            "<tr>",
                "<td>Second name:</td>",
                "<td><input id='sndname' type='text' name='sndname' autocomplete='off' /></td>",
            "</tr>",
            "<tr>",
                "<td>Email address:</td>",
                "<td><input id='email' type='text' name='email' autocomplete='off' /></td>",
            "</tr>",
            "<tr>",
                "<td>Language:</td>",
                "<td>",
                    "<select id='lang' name='lang'>",
                        "<option value='LANG_SV'>Sweden (Swedish)</option>",
                        "<option value='LANG_EN'>Britain (English)</option>",
                    "</select>",
                "</td>",
            "</tr>",
            "<tr>",
                "<td>Header:</td>",
                "<td>",
                    "<input id='editor' type='button' class='editer' value='Default'/>",
                    "<span class='editable' name='custommessage' style='display: none'>"
                    + jQuery('#message').html() +
                    "</span>",
                "</td>",
            "</tr>",
        "</table>",
    "</div>"
].join("");

var showValidationError = function(t, element, validationError) {
    element.css("background", "red");
    jQuery(document.createElement('div'))
        .attr("name", "validate-message")
        .css({"font-size": 8, "font-weight": "bold", "color": "red"})
        .append(validationError.message())
        .appendTo(element.parent());
}

var onAccept = function() {
    var fstname = $('#fstname');
    var sndname = $('#sndname');
    var email = $('#email');
    var lang = $('#lang');

    // Reset red (error) backgrounds
    _.map([fstname, sndname, email, lang], function(e) {
        e.css("background", "white");
    });

    // Delete error messages
    jQuery('[name=validate-message]').remove();

    var vresult = [
        fstname.validate(new NameValidation({callback: showValidationError, message: "Wrong first name format!"})),
        sndname.validate(new NameValidation({callback: showValidationError, message: "Wrong second name format!"})),
        email.validate(new NotEmptyValidation({callback: showValidationError, message: "Email cannot be empty!"})).concat(new EmailValidation({callback: showValidationError, message: "Email is not valid!"})),
        lang.validate(new NotEmptyValidation({callback: showValidationError, message:"Language cannot be empty!"}))
            ];

    // Check all validations and submit if nothing crazy is going on.
    if (vresult.every(function(a) {return a;})) {
        new Submit({
            method: "POST",
            add : "true",
            url: "/adminonly/createuser",
            fstname : fstname.val(),
            sndname : sndname.val(),
            email : email.val(),
            lang : lang.val(),
            custommessage : jQuery('#account-body textarea').val()
        }).send();
    }
}

window.CreateUserButton = 
    Button.init({
        color: "green",
        size: "tiny",
        text: "Create user",
        onClick: function() {
            Confirmation.popup({
              onAccept: onAccept,
              title: "Create user",
              acceptButtonText: "OK",
              content: form
            });

            // Set up editor
            $('#editor').click(function(){
                prepareForEdit(jQuery(this).parents("div"), '300');
                jQuery(this).hide();
            });
        }
    });

})(window);
