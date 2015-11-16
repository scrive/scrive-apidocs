define(["legacy_code", "Underscore", "Backbone", "React", "common/button", "common/render_localization",
  "signview/tasks/task_mixin"],
  function (legacy_code, _, Backbone, React, Button, RenderLocalization, TaskMixin) {

  return React.createClass({
    mixins: [TaskMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      name: React.PropTypes.string.isRequired,
      canSign: React.PropTypes.bool.isRequired,
      onReject: React.PropTypes.func.isRequired,
      onSign: React.PropTypes.func.isRequired
    },

    createTasks: function () {
      var self = this;
      var model = self.props.model;

      return [new PageTask({
        type: "sign",
        onArrowClick: function () {
          self.props.onSign();
        },
        isComplete: function () {
          return !model.document().currentSignatoryCanSign();
        },
        el:  $(self.refs.signButton.getDOMNode()),
        onActivate: function () {
          mixpanel.track("Begin signature task");
        },
        onDeactivate: function () {
          mixpanel.track("Finish signature task");
        }
      })];
    },

    handleSign: function () {
      if (this.props.canSign) {
        this.props.onSign();
      } else {
        this.props.model.arrow().blink();
      }
    },

    render: function () {
      var model = this.props.model;
      var canHaveRejectButton = model.hasRejectOption();
      var name = this.props.name;

      var buttonClass = React.addons.classSet({
        "button-block": true,
        "inactive": !this.props.canSign
      });

      var divClass = React.addons.classSet({
        "col-xs-6": !BrowserInfo.isSmallScreen(),
        "col-xs-12": BrowserInfo.isSmallScreen(),
        "center-block": true
      });

      return (
        <div className={divClass}>
          <h1>{localization.process.signModalTitle}</h1>
          {/* if */ name !== "" &&
            <RenderLocalization
              subs={{"put-signatory-name-here": name}}
              text={localization.process.signModalBody}
            />
          }
          {/* if */ name === "" &&
            <RenderLocalization
              text={localization.process.signModalBodyNoName}
            />
          }
          <Button
            type="action"
            ref="signButton"
            className={buttonClass}
            onClick={this.handleSign}
            text={localization.process.signbuttontextfromsignaturedrawing}
          />
          {/* if */ canHaveRejectButton &&
            <Button
              ref="rejectButton"
              className="transparent-button button-block"
              text={localization.process.rejectbuttontext}
              onClick={this.props.onReject}
            />
          }
        </div>
      );
    }
  });
});
