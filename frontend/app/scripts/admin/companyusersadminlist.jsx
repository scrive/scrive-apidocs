/** @jsx React.DOM */

define(['React', 'lists/list', 'moment', 'legacy_code'], function(React, List, moment) {


var openNewUserModal = function(companyid,callback) {
  var body = jQuery("<div class='standard-input-table'>");
  body.append("<p>Create new user account in company</p>");
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
  var tr7 = jQuery("<tr/>").append(jQuery("<td/>").text("Is company admin:"));
  var isadmin = jQuery("<input type='checkbox' name='isadmin' autocomplete='off' />");
  tr7.append(jQuery("<td/>").append(isadmin));
  table.append(tr7);

  body.append(table);

  var confirmation = new Confirmation({
    title : "Create user in company",
    acceptText : "Create",
    content  : body,
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
        var successCallback = function(resp) {
          resp = JSON.parse(resp);
          if (resp.success) {
            confirmation.close();
            callback();
          } else {
            new FlashMessage({color: 'red', content: resp.error_message});
          }
        };
        new Submit({
          method: "POST",
          url: "/adminonly/companyadmin/users/" + companyid,
          fstname : fstname.val(),
          sndname : sndname.val(),
          email : email.val(),
          lang : lang.val(),
          iscompanyadmin : isadmin.is(":checked") ? "Yes" : undefined,
        }).sendAjax(successCallback, function() {window.location.reload();});
      }
    }
  });
};

return React.createClass({
    mixins : [List.ReloadableContainer],
    openDeleteInvitationModal : function(d) {
      var self = this;
      var popup = new Confirmation({
        title : "Delete invitation",
        acceptText: "Delete",
        content : $("<div style='text-align:center;'>Are you sure that you want to delete this invitation?<BR/>"
                     + " User will still exist in his original company.</div>"),
        onAccept : function() {
          new Submit({
            url: "/adminonly/useradmin/deleteinvite/" + self.props.companyid + "/" + d.field("fields").id,
            method: "POST",
            ajaxsuccess: function() {
              popup.clear();
              self.reload();
              return false;
            },
            ajaxerror: function() {
              new FlashMessage({color: "red", content : "Failed"});
              return false;
            }
          }).sendAjax();
        }
      });
    },
    render: function() {
      var self = this;
      return (
        <List.List
          url={"/adminonly/companyaccounts/" + self.props.companyid}
          dataFetcher={function(d) {return d.list;}}
          idFetcher={function(d) {return d.field("fields").id;}}
          loadLater={self.props.loadLater}
          ref='list'
        >
         <List.TextFiltering text={"Company Accounts" } />

         <List.ListAction
            name="Add new user in company"
            color="green"
            size="tiny"
            onSelect={function() {
             openNewUserModal(self.props.companyid, function() {self.reload();});
            }}
          />

         <List.Column
           name="Name"
           width="100px"
           sorting="fullname"
           rendering={function(d) {
             return (<a href={d.field("link")}>{d.field("fields").fullname}</a>);
           }}
         />
         <List.Column
           name="Email"
           width="100px"
           sorting="email"
           rendering={function(d) {
              return (<a href={d.field("link")}>{d.field("fields").email}</a>);
           }}
         />

         <List.Column
           name="Role"
           width="100px"
           sorting="role"
           rendering={function(d) {
             if (d.field("fields").role =="RoleAdmin") {
               return (<span>Admin</span>);
             } else if (d.field("fields").role =="RoleStandard") {
               return (<span>Standard</span>);
             } else if (d.field("fields").role =="RoleInvite") {
               return (<span>!User in different company!</span>);
             } else {
               return (<span/>);
             }
           }}
         />

         <List.Column
           name="TOS date"
           width="100px"
           rendering={function(d) {
             if (d.field("fields").role =="RoleInvite") {
               return (<a style={{color:"red"}} href="#" onClick={function() {self.openDeleteInvitationModal(d)}}> Click to delete this invitation </a>);
             }
             var time = d.field("fields").tos;
             if (time != undefined && time != "") {
               return (<small> {moment(time).toDate().toTimeAbrev()}</small>);
             } else
               return (<span/>);
           }}
         />


         <List.Pagination/>
       </List.List>
     );
   }
});

});
