define(["legacy_code", "Underscore", "Backbone", "React", "common/backbone_mixin", "common/button"],
  function (legacy_code, _, Backbone, React, BackboneMixin, Button) {

  return React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      noScreenshot: React.PropTypes.bool
    },

    getBackboneModels: function () {
      return [this.props.model];
    },

    componentDidMount: function () {
      var doc = this.props.model.document();
      this.sendTrackingData();

      /* istanbul ignore next */
      if (!this.props.noScreenshot) {
        setTimeout(function () {
          doc.takeFirstScreenshot();
        }, 1500);
      }
    },

    signButtonNode: function () {
      return this.refs.signButton.getDOMNode();
    },

    sendTrackingData: /* istanbul ignore next */ function () {
      var signatory = this.props.model.document().currentSignatory();

      var sps = {};
      sps["Has user?"] = signatory.hasUser();
      sps["First visit"] = !signatory.seendate();
      mixpanel.register(sps);

      var ps = {};
      ps["Full Name"] = signatory.nameOrEmail();
      ps.$email = signatory.email();
      if (signatory.fstname()) {
        ps.$first_name = signatory.fstname();
      }

      if (signatory.sndname()) {
        ps.$last_name = signatory.sndname();
      }

      if (signatory.hasUser()) {
        ps.$username = signatory.email();
      }

      mixpanel.people.set(ps);

      mixpanel.track("View sign view");
    },

    activateSignConfirmation: function () {
      var model = this.props.model;
      var signatoryHasPlacedSignatures = model.document().currentSignatory().hasPlacedSignatures();

      var valid = model.tasks().notCompletedTasks().length == 1 &&
        model.tasks().notCompletedTasks()[0].isSignTask();

      if (!valid) {
        model.arrow().blink();
        return ;
      }

      mixpanel.track("Click sign");

      new DocumentSignConfirmation({
        model: model,
        signview: true,
        signaturesPlaced: signatoryHasPlacedSignatures
      });
    },

    handleReject: function () {
      var model = this.props.model;
      var doc = model.document();

      mixpanel.track("Click Reject");

      var arrow = model.arrow();
      if (arrow) { arrow.disable(); }
      var content = $("<div />");
      var textarea = $("<textarea class='modal-textarea' />");
      textarea.attr("placeholder", localization.process.signatorycancelmodalplaceholder);
      var text = $("<p style='margin-bottom: 10px'/>");
      text.text(localization.process.signatorycancelmodaltext);

      content.append(text);
      content.append(textarea);

      var rejectErrorCallback = function (xhr) {
        if (xhr.status == 403) {
          ScreenBlockingDialog.open({header: localization.sessionTimedoutInSignview});
        } else {
          new FlashMessage({content: localization.signviewSigningFailed, type: "error", withReload: true});
        }
      };

      var popup = new Confirmation({
        title: localization.process.signatorycancelmodaltitle,
        icon: null,
        width: 500,
        content: content,
        extraClass: "grey reject-modal",
        acceptText: localization.reject.send,
        acceptColor: "cancel",
        onAccept: function () {
          ReloadManager.stopBlocking();
          var customtext = textarea.val();
          trackTimeout("Accept", {"Accept": "reject document"}, function () {
            var text = customtext != undefined && customtext != "" ? customtext : undefined;
            doc.currentSignatory().reject(text).sendAjax(function () {
              var shouldRedirect = doc.currentSignatory().rejectredirect() != undefined &&
                doc.currentSignatory().rejectredirect() != "";
              if (shouldRedirect) {
                window.location = doc.currentSignatory().rejectredirect();
               } else {
                window.location.reload();
               }
            }, rejectErrorCallback);
          });
        },
        onReject: function () {
          var arrow = model.arrow();
          if (arrow) { arrow.enable(); }
        }
      });
    },

    handleSign: function () {
      this.activateSignConfirmation();
    },

    render: function () {
      var self = this;
      var model = this.props.model;
      var doc = model.document();
      var sig = doc.currentSignatory();
      var hasPlacedSigs = sig.hasPlacedSignatures();

      var canHaveRejectButton = model.hasRejectOption() && !BrowserInfo.isSmallScreen();

      var signButtonStyle = {};
      var signWrapperStyle = {};
      if (BrowserInfo.isSmallScreen()) {
        signButtonStyle = {
          paddingLeft: "0px",
          paddingRight: "0px",
          fontSize: "100px",
          height: "100px",
          maxHeight: "120px",
          lineHeight: "85px",
          paddingTop: "55px",
          paddingBottom: "55px",
          margin: "0px",
          width: "100%"
        };

        signWrapperStyle = {
          width: "100%",
          marginRight: "0px"
        };
      }

      var signButton = (
        <div style={signWrapperStyle} className="signwrapper sign">
          <Button
            ref="signButton"
            type="action"
            style={signButtonStyle}
            className="sign-button"
            text={hasPlacedSigs ? localization.next : localization.process.signbuttontext}
            onClick={this.handleSign}
          />
        </div>
      );

      var boxStyle = {};
      if (!canHaveRejectButton) {
        boxStyle.textAlign = "center";
      }
      if (BrowserInfo.isSmallScreen()) {
        boxStyle.padding = "0px";
        boxStyle.margin = "10px auto auto";
        boxStyle.width = "939px";
      }

      return (
        <span style={boxStyle}>
          {/* if */ canHaveRejectButton &&
            <span>
              <div className="rejectwrapper reject">
                <Button
                  ref="rejectButton"
                  text={localization.process.rejectbuttontext}
                  onClick={this.handleReject}
                />
              </div>
              {signButton}
            </span>
          }
          {/* else */ !canHaveRejectButton &&
            signButton
          }
          <div className="clearfix"/>
        </span>
      );
    }
  });
});
