/* Helper methods to create a popup where user information is filled in. */

(function(window) {
window.CreateUserButton = Button.init({
        color: "green",
        size: "tiny",
        text: "Create user",
        onClick: function() {
            var body = jQuery("<div class='account-body'>");
            body.append("<p>Create new user account</p>");
            var table = jQuery("<table/>");

            var tr1 = jQuery("<tr/>").append(jQuery("<td/>").text("First name:"));
            var fstname = jQuery("<input type='text' name='fstname' autocomplete='off' />");
            tr1.append(jQuery("<td/>").append(fstname));
            table.append(tr1);

            var tr2 = jQuery("<tr/>").append(jQuery("<td/>").text("Second name:"));
            var sndname = jQuery("<input type='text' name='sndname' autocomplete='off' />");
            tr2.append(jQuery("<td/>").append(sndname));
            table.append(tr2);

            var tr5 = jQuery("<tr/>").append(jQuery("<td/>").text("Email address:"));
            var email = jQuery("<input type='text' name='email' autocomplete='off' />");
            tr5.append(jQuery("<td/>").append(email));
            table.append(tr5);

            var tr6 = jQuery("<tr/>").append(jQuery("<td/>").text("Language:"));
            var lang = jQuery("<select name='lang'>"
                                + "<option value='LANG_SV' selected=''>Sweden (Swedish)</option>"
                                + "<option value='LANG_EN'>Britain (English)</option>"
                                + "</select>");
            tr6.append(jQuery("<td/>").append(lang));
            table.append(tr6);

            var tr7 = jQuery("<tr/>").append(jQuery("<td/>").text("Header:"));
            var btnHeader = jQuery('<input type="button" class="editer" value="Default"/>').click(function(){
                prepareForEdit(jQuery(this).parents("div"), '300');
                jQuery(this).hide();
            });
            var header = jQuery('<span class="editable" name="custommessage" style="display: none"></span>');

            tr7.append(jQuery("<td/>").append(btnHeader).append(header));
            table.append(tr7);

            body.append(table);


            Confirmation.popup({
              onAccept : function() {
                            var callback = function(t,e,v) {
                                e.css("background", "red");
                                jQuery(document.createElement('div'))
                                    .attr("name", "validate-message")
                                    .css({"font-size": 8, "font-weight": "bold", "color": "red"})
                                    .append(v.message())
                                    .appendTo(e.parent());
                            };

                            //reset red background
                            fstname.css("background", "white");
                            sndname.css("background", "white");
                            email.css("background", "white");
                            lang.css("background", "white");

                            //delete messages
                            jQuery('[name=validate-message]').remove();

                            var vresult = [
                                fstname.validate(new NameValidation({callback: callback, message: "Wrong first name format!"})),
                                sndname.validate(new NameValidation({callback: callback, message: "Wrong second name format!"})),
                                email.validate((new NotEmptyValidation({callback: callback, message: "Email cannot be empty!"})).concat(new EmailValidation({callback: callback}))),
                                lang.validate(new NotEmptyValidation({callback: callback, message:"Language cannot be empty!"}))
                                ];

                            if (vresult.every(function(a) {return a;})) {
                                new Submit({
                                    method: "POST",
                                    add : "true",
                                    url: "/adminonly/createuser",
                                    fstname : fstname.val(),
                                    sndname : sndname.val(),
                                    email : email.val(),
                                    lang : lang.val(),
                                    custommessage : jQuery('textarea',body).val()
                                    }).send();
                            }
                         },
              title : "Create user",
              acceptButtonText : "OK",
              content  : body
            });
        }
    });
})(window);
