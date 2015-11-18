/** @jsx React.DOM */

define(["React", "Backbone", "legacy_code", "common/button"], function (React, Backbone, _legacy, Button) {

  return React.createClass({
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

    makeTOSCopyWithLink: function () {
      var res = $("<span>" + localization.docsignview.acceptTOS + "</span>");
      $(".is-TOS", res)
        .addClass("terms clickable")
        .attr("target", "_blank")
        .attr("href", "/terms");
      return res.html();
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
          <div className="col-xs-6">
            <h1>{localization.docsignview.titleText}</h1>
            <p>{localization.docsignview.subtitleText}</p>
          </div>
          <div className="col-xs-6 right">
            <Button
              type="action"
              text={localization.docsignview.signupButtonText}
              onClick={this.registerUser}
            />
            <p className="bottom-label" dangerouslySetInnerHTML={{__html: this.makeTOSCopyWithLink()}} />
          </div>
        </div>
      );
    }
  });
});
