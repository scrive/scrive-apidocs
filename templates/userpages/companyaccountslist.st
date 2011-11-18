####    Pages for users - usual from my account
viewCompanyAccounts =
<div>
  <div id="tab-viewer">
      <div class="tab-header"></div>
      <ul class="tabs">
        <li><a href="/account">$_HeadUserInfo()$</a></li>
        <li><a href="/account/security">$_HeadAccountSettings()$</a></li>
        <li class="active"><a href="/account/companyaccounts">$_CompanyAccounts()$</a></li>
        <li><a href="/account/sharing">$_sharing()$</a></li>
        <li><a href="/account/mailapi">$_HeadMailApi()$</a></li>
      </ul>
  </div>
</div>

<script>
 jQuery(function(){
  var addCompanyButton = Button.init({
        color: "green",
        size: "tiny",
        text: "$_newCompanyAccountButtonText()$",
        onClick: function() {
            var body = jQuery("<div class='account-body'>");
            var table = jQuery("<table/>");

            var tr1 = jQuery("<tr/>").append(jQuery("<td/>").text("$_firstname()$:"));
            var fstname = jQuery("<input type='text' name='fstname' autocomplete='off' />");
            tr1.append(jQuery("<td/>").append(fstname));
            table.append(tr1);

            var tr2 = jQuery("<tr/>").append(jQuery("<td/>").text("$_lastname()$:"));
            var sndname = jQuery("<input type='text' name='sndname' autocomplete='off' />");
            tr2.append(jQuery("<td/>").append(sndname));
            table.append(tr2);

            var tr3 = jQuery("<tr/>").append(jQuery("<td/>").text("$_phone()$:"));
            var phone = jQuery("<input type='text' name='phone' autocomplete='off' />");
            tr3.append(jQuery("<td/>").append(phone));
            table.append(tr3);

            var tr4 = jQuery("<tr/>").append(jQuery("<td/>").text("$_companyposition()$:"));
            var companyposition = jQuery("<input type='text' name='companyposition' autocomplete='off' />");
            tr4.append(jQuery("<td/>").append(companyposition));
            table.append(tr4);

            var tr5 = jQuery("<tr/>").append(jQuery("<td/>").text("$_email()$"));
            var email = jQuery("<input type='text' name='email' autocomplete='off' />");
            tr5.append(jQuery("<td/>").append(email));
            table.append(tr5);

            body.append(table);


            Confirmation.popup({
              onAccept : function() {
                            new Submit({
                                method: "POST",
                                add : "true",
                                fstname : fstname.val(),
                                sndname : sndname.val(),
                                phone : phone.val(),
                                companyposition : companyposition.val(),
                                email : email.val(),
                                }).send();
                         },
              title : "$_modalAddCompanyAccountTitle()$",
              acceptButtonText : "$_modalAddCompanyAccountInviteButtonText()$",
              content  : body
            });
            }
      });


  var companyAccountsTable = KontraList.init({
    name : "CompanyAccountsTable",
    schema: new Schema({
      url: "/companyaccounts",
      sorting: new Sorting({ fields: ["firstname",
                                      "lastname",
                                      "position",
                                      "phone",
                                      "email"] }),
      paging: new Paging({}),
      filtering: new Filtering({ text: "", infotext: "$_SearchCompanyAccount()$" }),
      cells : [
        new Cell({name: "ID", width:"20px", field:"id", special: "select"}),
        new Cell({name: "$_firstname()$", width:"65px", field:"firstname"}),
        new Cell({name: "$_lastname()$", width:"80px", field:"lastname" }),
        new Cell({name: "$_companyposition()$", width:"50px", field:"position" }),
        new Cell({name: "$_phone()$", width:"105px", field:"phone" }),
        new Cell({name: "$_email()$", width:"100px", field:"email" })
      ],
      options:
        [{ name: "$_delete()$",
           onSelect: function(users) {
              var submit = new Submit({
                                url: "$currentlink$",
                                method: "POST",
                                remove: "true",
                                archive: "true",
                                acccheck: _.map(users, function(user) { return user.field("id"); })
                          });
              var text = "$_listLocalDialogsBody()$"
              if (users.length == 1) {
                var fullname = users[0].field("firstname") + " " + users[0].field("lastname");
                if (fullname.length<2)
                  fullname = users[0].field("email");
                text += fullname + ".";
              } else {
                text += users.length + (" $_CompanyAccounts()$.").toLowerCase();
              }
              var content = jQuery("<p/>").text(text);
              Confirmation.popup({
                submit: submit,
                acceptText: "$_modalAddCompanyAccountInviteButtonText()$",
                rejectText: "$_cancel()$",
                title: "$_delete()$",
                content: content
              })
           }
         }]
    }),
    headerExtras: addCompanyButton.input()
  });
    jQuery("#tab-viewer").append(companyAccountsTable.view.el);
  });
</script>
#####