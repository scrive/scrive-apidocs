var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var Button = require("../../common/button");
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");
var TaskMixin = require("../tasks/task_mixin");
var ViewSize = require("../viewsize");
var PageTask = require("../../../js/tasks.js").PageTask;
var $ = require("jquery");

  module.exports = React.createClass({
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
      var hasSignaturesPlaced = this.props.model.document().currentSignatory().hasPlacedSignatures();

      var divClass = React.addons.classSet({
        "col-xs-6": !ViewSize.isSmall(),
        "col-xs-12": ViewSize.isSmall(),
        "center-block": true
      });

      return (
        <div className={divClass}>
          <h1>{hasSignaturesPlaced ? localization.process.signModalTitle : localization.process.signbuttontext}</h1>
          <p>
            <HtmlTextWithSubstitution
              secureText={hasSignaturesPlaced ? localization.signviewConfirmationSignaturesPlaced :
                                                localization.signviewConfirmation}
              subs={{".put-document-title-here": this.props.title, ".put-signatory-name-here": this.props.name}}
            />
          </p>
          <Button
            type="action"
            ref="signButton"
            className="button-block"
            onClick={this.props.onSign}
            text={hasSignaturesPlaced ? localization.process.signbuttontextfromsignaturedrawing :
                                        localization.process.signbuttontext}
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
