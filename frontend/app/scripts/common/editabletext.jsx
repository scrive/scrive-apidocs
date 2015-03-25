/** @jsx React.DOM */

/**
 * An editable text component. Click on a text and a input box appears.
 *
 * Properties:
      text        : string, the text that can be edited.
      edit        : mount the component in the editing state.
      disabled    : disable editing.
      onSave      : function, callback with text as argument when save button is clicked,
                    return true if it is valid input.
 *
 */

define(["React", "common/button", "common/infotextinput", "legacy_code"], function (React, Button, InfoTextInput) {
  var EditableText = React.createClass({
    mixins: [React.addons.PureRenderMixin],

    propTypes: {
      text: React.PropTypes.string.isRequired,
      onSave: React.PropTypes.func.isRequired,
      edit: React.PropTypes.bool,
      disabled: React.PropTypes.bool
    },

    getInitialState: function () {
      return {edit: false};
    },

    componentWillMount: function () {
      if (this.props.edit) {
        this.setState({edit: true});
      }
    },

    componentDidMount: function () {
      if (this.state.edit) {
        this.refs.input.focus();
      }
    },

    componentWillReceiveProps: function (nextProps) {
      if (this.state.edit && nextProps.disabled) {
        this.text();
      }
    },

    componentDidUpdate: function (prevProps, prevState) {
      if (this.state.edit && !prevState.edit) {
        this.refs.input.focus();
      }
    },

    edit: function () {
      this.setState({
        edit: true,
        text: this.props.text
      });
    },

    text: function () {
      return this.setState({
        edit: false,
        invalid: false,
        text: ""
      });
    },

    save: function () {
      var text = this.refs.input.value().trim();
      var valid = this.props.onSave(text);

      if (valid) {
        return this.text();
      }

      this.refs.input.focus();
    },

    handleClick: function () {
      if (this.state.edit || this.props.disabled) {
        return ;
      }

      this.edit();
    },

    render: function () {
      var cursorStyle = {
        "cursor": "pointer"
      };

      if (this.props.disabled) {
        cursorStyle.cursor = "";
      }

      if (this.state.edit) {
        cursorStyle.cursor = "text";
      }

      return (
        <div style={cursorStyle} className="editable-text" onClick={this.handleClick}>
          {/* if */ this.state.edit &&
            <span>
              <span className="editable-text-input-container">
                <InfoTextInput
                  className="editable-text-input"
                  value={this.props.text}
                  ref="input"
                />
              </span>
              <Button
                size="tiny"
                className="editable-text-save"
                text={localization.save}
                type="action"
                onClick={this.save}
              />
            </span>
          }
          {/* else */ !this.state.edit &&
            <span className="editable-text-text">
              {this.props.text}
              {/* if */ !this.props.disabled &&
                <img className="editable-text-icon" src="/img/edit-icon.png" />
              }
            </span>
          }
        </div>
      );
    }
  });

  return EditableText;
});
