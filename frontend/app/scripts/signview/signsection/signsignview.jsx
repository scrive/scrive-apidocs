define(["legacy_code", "Underscore", "Backbone", "React", "common/backbone_mixin", "common/button",
  "signview/tasks/task_mixin", "signview/is_small_view"],
  function (legacy_code, _, Backbone, React, BackboneMixin, Button, TaskMixin, isSmallView) {

  return React.createClass({
    mixins: [BackboneMixin.BackboneMixin, TaskMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      canSign: React.PropTypes.bool.isRequired,
      onSign: React.PropTypes.func.isRequired,
      onReject: React.PropTypes.func.isRequired
    },

    getBackboneModels: function () {
      return [this.props.model];
    },

    createTasks: function () {
      var self = this;
      var model = self.props.model;

      return [new PageTask({
        type: "sign",
        onArrowClick: function () {
          self.handleSign();
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
      var self = this;
      var model = this.props.model;
      var doc = model.document();
      var sig = doc.currentSignatory();
      var hasPlacedSigs = sig.hasPlacedSignatures();

      var canHaveRejectButton = model.hasRejectOption();

      var buttonClass = React.addons.classSet({
        "button-block": true,
        "inactive": !this.props.canSign
      });

      var divClass = React.addons.classSet({
        "col-xs-6": !isSmallView(),
        "col-xs-12": isSmallView(),
        "center-block": true
      });

      return (
        <div className={divClass}>
          <Button
            ref="signButton"
            type="action"
            className={buttonClass}
            text={localization.next}
            onClick={this.handleSign}
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
