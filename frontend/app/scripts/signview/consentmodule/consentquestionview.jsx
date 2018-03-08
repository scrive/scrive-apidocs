var React = require("react");
var _ = require("underscore");
var $ = require("jquery");
var Task = require("../navigation/task");
var TaskMixin = require("../navigation/task_mixin");
var RadioButton = require("../../icons/radiobutton");
var Button = require("../../common/button");
var classNames = require("classnames");
var ConsentQuestion = require("../../../js/consentquestion").ConsentQuestion;
var FieldPlacementGlobal = require("../../../js/fieldplacementglobal.js").FieldPlacementGlobal;
var Modal = require("../../common/modal");

module.exports = React.createClass({
  displayName: "ConsentQuestionView",
  mixins: [TaskMixin],

  propTypes: {
    model: React.PropTypes.instanceOf(ConsentQuestion).isRequired
  },

  getInitialState: function() {
    return { showDescription: false };
  },

  createTasks: function() {
    var self = this;
    var options= [[true, "positive"], [false, "negative"]];

    return _.map(options, function([response, name]) {
      return new Task({
        type: "consent-question",
        isComplete: function() {
          return self.props.model.hasResponse();
        },
        onArrowClick: self.onClick(response),
        tipSide: "left",
        el: $(self.getDOMNode()),
        pointSelector: "." + name + " .radiobutton",
        margin: -17,
        consentQuestion: self.props.model
      });
    });
  },

  onMouseDown: function(event) {
    event.stopPropagation();
    event.preventDefault();
  },

  onClick: function(response) {
    var self = this;
    return function() {
      self.props.model.setResponse(response);
    };
  },

  onShowHideClick: function() {
    this.setState({ showDescription: !this.state.showDescription });
  },

  render: function() {
    var self = this;
    var model = this.props.model;

    var options = [
      [true,  model.positiveOption(), model.isResponsePositive()],
      [false, model.negativeOption(), model.isResponseNegative()]
    ];

    var [positive, negative] = _.map(options, function([response, text, selected]) {
      var wrapperClass = classNames("consent-option", {
        positive: response,
        negative: !response
      });

      return (
        <div onMouseDown={self.onMouseDown} onClick={self.onClick(response)}
             className={wrapperClass}>
          <div className="clickable"></div>
          {/* if */ !model.hasResponse() &&
            <div className="underlay"><span></span></div>
          }
          <RadioButton
            selected={selected} active={true} inline={true}
            wrel={FieldPlacementGlobal.smallRadiobuttonRatio}
            pageWidth={1040} />
          <label>{text}</label>
        </div>
      );
    });

    var questionClass = classNames({
      "col-sm-9": model.hasDetailedDescription()
    });

    return (
      <div>
        <div className="consent-question row">
          <div className={questionClass}>
            <h2 className="title">{model.title()}</h2>
            {positive}
            {negative}
          </div>
          {/* if */ model.hasDetailedDescription() &&
            <div>
              <div className="col-sm-3 right hide-button detailed-description-button">
                <Button
                  text={model.detailedDescriptionTitle()}
                  onClick={self.onShowHideClick} />
              </div>
              <Modal.Container active={this.state.showDescription}
                               onClose={this.onShowHideClick}>
                <Modal.Header
                  title={model.detailedDescriptionTitle()}
                  showClose={true} onClose={this.onShowHideClick}
                />
                <Modal.Content>
                  <div className="consent-question-detailed-description">
                    {model.detailedDescriptionText()}
                  </div>
                </Modal.Content>
              </Modal.Container>
            </div>
          }
        </div>
      </div>
    );
  }
});
