define(["React", "common/button", "common/infotextinput"], function (React, Button, InfoTextInput) {
  return React.createClass({
    mixins: [React.addons.LinkedStateMixin],

    propTypes: {
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

    render: function () {
      var title = this.props.title;
      var subtitle = this.props.subtitle;
      var buttons = this.props.buttons;
      var onClick = this.props.onClick;
      var field = this.props.field;
      var fieldTitle = this.props.fieldTitle;
      var text = this.state.text;

      return (
        <span>
          <div className="col-xs-6 left">
            <h1>{title}</h1>
            {/* if */ subtitle && <p>{subtitle}</p>}
          </div>
          <div className="col-xs-6 right">
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
                return (
                  <Button
                    key={index}
                    type="action"
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
});
