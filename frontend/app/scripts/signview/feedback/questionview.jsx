var React = require("react");
var Button = require("../../common/button");
var InfoTextInput = require("../../common/infotextinput");
var ViewSize = require("../viewsize");

  module.exports = React.createClass({

    mixins: [React.addons.LinkedStateMixin],

    propTypes: {
      question: React.PropTypes.string.isRequired,
      title: React.PropTypes.string.isRequired,
      subtitle: React.PropTypes.string,
      onClick: React.PropTypes.func.isRequired,
      buttons: React.PropTypes.array.isRequired,
      field: React.PropTypes.string,
      fieldTitle: React.PropTypes.string
    },

    getInitialState: function () {
      return {text: ""};
    },

    handleChange: function (value) {
      this.setState({text: value});
    },
    componentDidMount: function () {
      $(window).on("resize", this.handleResize);
    },
    componentWillUnmount: function () {
      $(window).off("resize", this.handleResize);
    },
    handleResize: function (e) {
      this.forceUpdate();
    },
    render: function () {
      var self = this;
      var question = this.props.question;
      var title = this.props.title;
      var subtitle = this.props.subtitle;
      var buttons = this.props.buttons;
      if (ViewSize.isSmall()) {
        buttons = _.clone(buttons);
        buttons.reverse();
      }
      var onClick = this.props.onClick;
      var field = this.props.field;
      var fieldTitle = this.props.fieldTitle;
      var text = this.state.text;

      return (
        <span>
          <div className="col-sm-6 left">
            <h1>{title}</h1>
            {/* if */ subtitle && <p>{subtitle}</p>}
          </div>
          <div className="col-sm-6 right">
            {/* if */ field &&
              <div className="field">
                {/* if */ field === "textarea" &&
                  <span>
                    <p className="label"><label htmlFor="text">{fieldTitle}</label></p>
                    <textarea
                      id="text"
                      valueLink={this.linkState("text")}
                      className="signview-textarea"
                      placeholder={localization.process.signatorycancelmodalplaceholder}
                    />
                  </span>
                }
                {/* if */ field === "phone" &&
                  <span>
                    <p><label htmlFor="phone">{fieldTitle}</label></p>
                    <InfoTextInput
                      id="phone"
                      value={text}
                      onChange={this.handleChange}
                    />
                  </span>
                }
              </div>
            }
            <div className="button-group small-buttons">
              {buttons.map(function (button, index) {
                var realIndex = _.indexOf(self.props.buttons, button);
                var key = self.props.question + "-" + realIndex;
                return (
                  <Button
                    key={key}
                    type={button.type}
                    text={button.text}
                    onClick={function (e) { onClick(button.value, text); }}
                  />
                );
              })}
            </div>
          </div>
        </span>
      );
    }
  });
