/* Definition of user list seen by sales */

define(['Backbone', 'legacy_code'], function() {

    var downloadCSVButton = new Button({
        color: "green",
        size: "tiny",
        text: "Download list as CSV",
        onClick: function() {
          window.location.href = '/adminonly/paymentsstats.csv';
        }
    });

    var createUserButton =  new Button({
          color: "green",
          size: "tiny",
          text: "Create user with empty company",
          onClick: function() {
              var body = jQuery("<div class='standard-input-table'>");
              body.append("<p>Create new user account</p>");
              var table = jQuery("<table/>");

              var tr1 = jQuery("<tr/>").append(jQuery("<td/>").text("First name:"));
              var fstname = jQuery("<input type='text' name='fstname' autocomplete='off' />");
              tr1.append(jQuery("<td/>").append(fstname));
              var tr1ErrorRow = $('<tr class="validation-tr"><td/><td><div class="validation-failed-msg">Wrong first name format!</div></td></tr>');
              tr1ErrorRow.css('display', 'none');
              table.append(tr1).append(tr1ErrorRow);

              var tr2 = jQuery("<tr/>").append(jQuery("<td/>").text("Second name:"));
              var sndname = jQuery("<input type='text' name='sndname' autocomplete='off' />");
              tr2.append(jQuery("<td/>").append(sndname));
              var tr2ErrorRow = $('<tr class="validation-tr"><td/><td><div class="validation-failed-msg">Wrong second name format!</div></td></tr>');
              tr2ErrorRow.css('display', 'none');
              table.append(tr2).append(tr2ErrorRow);

              var tr5 = jQuery("<tr/>").append(jQuery("<td/>").text("Email address:"));
              var email = jQuery("<input type='text' name='email' autocomplete='off' />");
              tr5.append(jQuery("<td/>").append(email));
              var tr5ErrorRow = $('<tr class="validation-tr"><td/><td><div class="validation-failed-msg">Wrong email format!</div></td></tr>');
              tr5ErrorRow.css('display', 'none');
              table.append(tr5).append(tr5ErrorRow);

              var tr6 = jQuery("<tr/>").append(jQuery("<td/>").text("Language:"));
              var lang = jQuery("<select name='lang'>"
                                  + '<option value="sv">Swedish</option>'
                                  + '<option value="en">English</option>'
                                  + '<option value="de">German</option>'
                                  + '<option value="fr">French</option>'
                                  + '<option value="nl">Dutch</option>'
                                  + '<option value="it">Italian</option>'
                                  + '<option value="no">Norwegian</option>'
                                  + '<option value="pt">Portuguese</option>'
                                  + '<option value="es">Spanish</option>'
                                  + '<option value="da">Danish</option>'
                                  + '<option value="el">Greek</option>'
                                  + '<option value="fi">Finnish</option>'
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


              new Confirmation({
                onAccept : function() {
                              // Hide validation rows
                              tr1ErrorRow.css('display', 'none');
                              tr2ErrorRow.css('display', 'none');
                              tr5ErrorRow.css('display', 'none');

                              // Validate fields
                              var validationResult = true;
                              if(!new NameValidation().validateData(fstname.val())) {
                                  tr1ErrorRow.css('display','');
                                  validationResult = false;
                              }
                              if(!new NameValidation().validateData(sndname.val())) {
                                  tr2ErrorRow.css('display','');
                                  validationResult = false;
                              }
                              if(!new NotEmptyValidation().validateData(email.val())
                                 ||
                                 !new EmailValidation().validateData(email.val())
                                ) {
                                  tr5ErrorRow.css('display','');
                                  validationResult = false;
                              }

                              if (validationResult) {
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
                title : "Create user with empty company",
                acceptButtonText : "OK",
                content  : body
              });
          }
      });


    window.UserAdminSalesListDefinition = function() {

        return {
	    loadOnInit: false,
            name : "Users Table",
            schema: new Schema({
                url: "/adminonly/userslist",
                sorting: new Sorting({
                    fields: ["username", "email", "company", "phone", "tos"]
                }),
                paging: new Paging({}),
                textfiltering: new TextFiltering({text: "", infotext: "Username, email or company name."}),
                cells : [
                    new Cell({name: "Username", width:"130px", field:"username", special:"link"}),
                    new Cell({name: "Email", width:"130px", field:"email", special:"link"}),
                    new Cell({name: "Company", width:"100px", field:"company", special:"link"}),
                    new Cell({name: "Position", width:"100px", field:"companyposition", special:"link"}),
                    new Cell({name: "Phone", width:"100px", field:"phone", special:"link"}),
                    new Cell({name: "TOS date", width:"100px", field:"tos", special:"rendering",
                                    rendering: function(time) {
                                            if (time != undefined && time != "")
                                              return $("<small/>").text(new Date(Date.parse(time)).toTimeAbrev());
                                            else return $("<small/>");
                                    }
			    })
                ]
            }),
            headerExtras: function() {
                var buttons = $('<div></div>');
                buttons.append(createUserButton.el());
                buttons.append(downloadCSVButton.el());
                return buttons;
            }
        };
    };

});
