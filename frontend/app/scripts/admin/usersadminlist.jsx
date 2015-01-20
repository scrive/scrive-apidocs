/** @jsx React.DOM */

define(['React', 'lists/list', 'moment', 'legacy_code'], function(React, List, moment) {

var openCreateUserModal = function(callback) {
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

  body.append(table);

  var confirmation = new Confirmation({
    title : "Create user with empty company",
    acceptButtonText : "OK",
    content  : body,
    onAccept : function() {
      // Hide validation rows
      tr1ErrorRow.css('display', 'none');
      tr2ErrorRow.css('display', 'none');
      tr5ErrorRow.css('display', 'none');

      // Validate fields
      var validationResult = true;
      if(!new NameValidation().validateData(fstname.val()) && fstname.val() != "") {
        tr1ErrorRow.css('display','');
        validationResult = false;
      }
      if(!new NameValidation().validateData(sndname.val()) && sndname.val() != "") {
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
          add : "true",
          url: "/adminonly/createuser",
          fstname : fstname.val(),
          sndname : sndname.val(),
          email : email.val(),
          lang : lang.val()
        }).sendAjax(successCallback, function() {window.location.reload();});
      }
    }
 });
};


return React.createClass({
    mixins : [List.ReloadableContainer],
    render: function() {
      var self = this;
      return (
        <List.List
          url={"/adminonly/userslist"}
          dataFetcher={function(d) {return d.list;}}
          idFetcher={function(d) {return d.field("fields").id;}}
          loadLater={self.props.loadLater}
          ref='list'
        >
         <List.TextFiltering text={"Username, email or company name"} />

         <List.ListAction
            name="Create user with empty company"
            color="green"
            size="tiny"
            onSelect={function() {
             openCreateUserModal(function () { self.reload(); });
            }}
          />
         <List.ListAction
            name="Download list as CSV"
            color="green"
            size="tiny"
            onSelect={function() {
              window.location.href = '/adminonly/paymentsstats.csv';
            }}
          />

         <List.Column
           name="Username"
           width="130px"
           sorting="username"
           rendering={function(d) {
             return (<a href={d.field("link")}>{d.field("fields").username}</a>);
           }}
         />

         <List.Column
           name="Email"
           width="130px"
           sorting="email"
           rendering={function(d) {
             return (<a href={d.field("link")}>{d.field("fields").email}</a>);
           }}
         />

         <List.Column
           name="Company"
           width="100px"
           rendering={function(d) {
             return (<a href={d.field("link")}>{d.field("fields").company}</a>);
           }}
         />

         <List.Column
           name="Position"
           width="100px"
           sorting="companyposition"
           rendering={function(d) {
             return (<a href={d.field("link")}>{d.field("fields").companyposition}</a>);
           }}
         />

         <List.Column
           name="Phone"
           width="100px"
           sorting="phone"
           rendering={function(d) {
             return (<a href={d.field("link")}>{d.field("fields").phone}</a>);
           }}
         />

         <List.Column
           name="TOS date"
           width="100px"
           sorting="tos"
           rendering={function(d) {
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
