define(["legacy_code", "Underscore", "Backbone", "React", "common/backbone_mixin", "common/button"],
  function (legacy_code, _, Backbone, React, BackboneMixin, Button) {

  return React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      onMount: React.PropTypes.func
    },

    getBackboneModels: function () {
      return [this.props.model];
    },

    componentDidMount: function () {
      if (this.props.onMount) {
        this.props.onMount();
      }
    },

    signButtonNode: function () {
      return this.refs.signButton.getDOMNode();
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
      var arrow = model.arrow();

      new DocumentSignRejection({
        doc: doc,
        arrow: arrow
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

      var signButtonClass = React.addons.classSet({
        "sign-button": true,
        "sign-button-small": BrowserInfo.isSmallScreen()
      });

      var signWrapperClass = React.addons.classSet({
        "sign": true,
        "signwrapper": true,
        "signwrapper-no-reject": !canHaveRejectButton
      });

      var boxClass = React.addons.classSet({
        "section": true,
        "spacing": true,
        "signbuttons": true,
        "signbuttons-small": BrowserInfo.isSmallScreen()
      });

      return (
        <div className={boxClass}>
          {/* if */ canHaveRejectButton &&
            <div className="rejectwrapper reject">
              <Button
                ref="rejectButton"
                text={localization.process.rejectbuttontext}
                onClick={this.handleReject}
              />
            </div>
          }
          <div className={signWrapperClass}>
            <Button
              ref="signButton"
              type="action"
              className={signButtonClass}
              text={hasPlacedSigs ? localization.next : localization.process.signbuttontext}
              onClick={this.handleSign}
            />
          </div>
          <div className="clearfix"/>
        </div>
      );
    }
  });
});
