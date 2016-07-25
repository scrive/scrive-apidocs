var React = require("react");
var UploadButton = require("./uploadbutton");
var Submit = require("../../js/submits.js").Submit;
var FlashMessage = require("../../js/flashmessages.js").FlashMessage;
var $ = require("jquery");

/**
 * A upload button component in React. Internallly it uses UploadButton, and can take many UI propperties of buttons - but not all of them.
 * After upload, it sends file to server, and calls onUploadComplete with base64 representation of image
 *
 * Properties:
      text        : string , text on button,  default ""
      type:       : string, "action | optional | cancel | inactive | main",
      size        : string, tiny | small | big
      textcolor   : string, color for text on button, else default value for predefined color will be used.
      width       : integer, final width of button, if not set, button will adjust to text
      className   : additional css classes
      extensions  : list of strings of allowed file extensions
      unsupportedFileMessage: string with message to display when uploading failed due to filetype
      style       : style object (react format)

 *
 * Example usage:
 * var button = React.render(React.createElement(UploadButton,{
 *    text: "ABC"
 * }), div);
 *
 */

  module.exports = React.createClass({
    propTypes: {
      text        : React.PropTypes.string,
      type        : React.PropTypes.string,
      fileType    : React.PropTypes.string,
      size        : React.PropTypes.string,
      width       : React.PropTypes.number,
      className   : React.PropTypes.string,
      style       : React.PropTypes.object,
      extensions  : React.PropTypes.list,
      unsupportedFileMessage: React.PropTypes.string,
      onUploadComplete : React.PropTypes.func
    },
    getDefaultProps : function() {
      return {
        "className" : "",
        "text"      : "",
        "color"     : "green",
        "size"      : "small",
        "extensions": ["png", "jpg", "jpeg"],
        "unsupportedFileMessage": localization.imageFileFormatNotSpported,
        "style"     : {}
      };
    },
    onUploadComplete: function(input) {
      var self = this;
      var submit = new Submit({
        method: 'POST',
        url: '/serialize_image',
        extensions: self.props.extensions.join(","),
        ajax: true,
        ajaxerror: function (rs) {
          if (rs.status === 400) {
            new FlashMessage({type: "error", content: self.props.unsupportedFileMessage});
          }
        },
        ajaxsuccess: function (rs) {
          var logo_base64 = rs.logo_base64;
          self.props.onUploadComplete('data:image/png;base64,' + logo_base64);
        }
      });
      submit.addInputs($(input).attr("name", "logo"));
      setTimeout(function() {
        submit.sendAjax();
      },10);
    },
    render: function() {
      var self = this;
      return (
          <UploadButton
            text={this.props.text}
            type={this.props.type}
            fileType={this.props.fileType}
            size={this.props.size}
            width={this.props.width}
            className={this.props.className + " upload-button" }
            style={{position:"relative",overflow:"hidden"}}
            onUploadComplete={function(i) {
              self.onUploadComplete(i);
            }}
          />
      );
    }
  });
