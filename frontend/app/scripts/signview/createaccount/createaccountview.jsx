var React = require("react");
var Backbone = require("backbone");
var Button = require("../../common/button");
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");
var Document = require("../../../js/documents.js").Document;
var Submit = require("../../../js/submits.js").Submit;
var BrowserInfo = require("../../../js/utils/browserinfo.js").BrowserInfo;

  module.exports = React.createClass({
    propTypes: {
      document: React.PropTypes.instanceOf(Document).isRequired
    },
    registerUser: function () {
      var document = this.props.document;
      var currentSignatory = document.currentSignatory();
      new Submit({
        url: currentSignatory.saveurl(),
        method: "POST",
        tos: "on",
        email: currentSignatory.email(),
        ajax: true,
        ajaxsuccess: function (userObject) {
          userObjectParsed = JSON.parse(userObject);
          if (userObjectParsed.userid) {
            var userid = userObjectParsed.userid;
            mixpanel.alias(userid);
            mixpanel.identify(userid);
            mixpanel.track("Create new account",
                          {"Signup Method": "BySigning"});
            mixpanel.track("Sign TOS");

            var userData = {
              "TOS Date": new Date(),
              "Full Name": currentSignatory.name(),
              "$first_name": currentSignatory.fstname(),
              "$last_name": currentSignatory.sndname(),
              "$email": currentSignatory.email(),
              "Language": document.lang().simpleCode(),
              "Referring Company": document.author().company(),
              "Signup Method": "BySigning"
            };

            // Don't set company name to anything if not set, for easier filtering in MixPanel.
            if (currentSignatory.company()) {
              userData["Company name"] = currentSignatory.company();
            }

            // Register current user's data in Mixpanel
            mixpanel.register(userData); // as super property
            mixpanel.people.set(userData); // as regular person property
            window.location = "/d";
          }
        }
      }).send();
    },

    render: function () {
      var cx = React.addons.classSet;
      var mainContainerClasses = cx({
        "small-screen": BrowserInfo.isSmallScreen(),
        "save-backup-copy": true,
        "save": true,
        "section": true,
        "spacing": true
      });

      return (
        <div className="section safety-copy">
          <div className="col-sm-6">
            <h1>{localization.docsignview.titleText}</h1>
            <p>{localization.docsignview.subtitleText}</p>
          </div>
          <div className="col-sm-6 right">
            <Button
              type="action"
              text={localization.docsignview.signupButtonText}
              onClick={this.registerUser}
            />
            <p className="bottom-label">
              <HtmlTextWithSubstitution
                secureText={localization.docsignview.acceptTOS}
                classes={{".is-TOS": "terms clickable"}}
                links={{".is-TOS": "/terms"}}
              />
            </p>
          </div>
        </div>
      );
    }
  });
