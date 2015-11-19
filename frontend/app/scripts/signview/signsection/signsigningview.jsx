define(["legacy_code", "Underscore", "Backbone", "React", "common/button", "common/render_localization",
  "signview/tasks/task_mixin"],
  function (legacy_code, _, Backbone, React, Button, RenderLocalization, TaskMixin) {

  return React.createClass({
    mixins: [TaskMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      title: React.PropTypes.string.isRequired,
      name: React.PropTypes.string.isRequired,
      canSign: React.PropTypes.bool.isRequired,
      onBack: React.PropTypes.func.isRequired,
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
        el: $(self.refs.signButton.getDOMNode()),
        onActivate: function () {
          mixpanel.track("Begin signature task");
        },
        onDeactivate: function () {
          mixpanel.track("Finish signature task");
        }
      })];
    },

    render: function () {
      var divClass = React.addons.classSet({
        "col-xs-6": !BrowserInfo.isSmallScreen(),
        "col-xs-12": BrowserInfo.isSmallScreen(),
        "center-block": true
      });

      return (
        <div className={divClass}>
          <h1>{localization.process.signbuttontext}</h1>
          <p>
            <RenderLocalization
              subs={{"put-document-title-here": this.props.title, "put-signatory-name-here": this.props.name}}
              text={localization.signviewConfirmation}
            />
          </p>
          <Button
            type="action"
            ref="signButton"
            className="button-block"
            onClick={this.props.onSign}
            text={localization.process.signbuttontext}
          />
          <Button
            className="transparent-button button-block"
            onClick={this.props.onBack}
            text={localization.toStart.backFromSigningPage}
          />
        </div>
      );
    }
  });
});
