/* Definition of user list seen by sales */
(function(window){

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

                              if (_.every(vresult, function(a) {return a;})) {
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
                    new Cell({name: "TOS date", width:"100px", field:"tos", special:"link",
                                    rendering: function(time, idx, doc) {
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
})(window);
