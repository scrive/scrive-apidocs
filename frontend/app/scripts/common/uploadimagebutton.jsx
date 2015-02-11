/** @jsx React.DOM */

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
      className    : additional css classes
      style       : style object (react format)

 *
 * Example usage:
 * var button = React.render(UploadButton({
 *    text: "ABC"
 * }), div);
 *
 */

define(['React','common/uploadbutton'], function(React,UploadButton) {
  return React.createClass({
    propTypes: {
      text        : React.PropTypes.string,
      type        : React.PropTypes.string,
      fileType    : React.PropTypes.string,
      size        : React.PropTypes.string,
      width       : React.PropTypes.number,
      className   : React.PropTypes.string,
      style       : React.PropTypes.object,
      onUploadComplete : React.PropTypes.func
    },
    getDefaultProps : function() {
      return {
        "className" : "",
        "text"      : "",
        "color"     : "green",
        "size"      : "small",
        "style"     : {}
      };
    },
    onUploadComplete: function(input) {
      var self = this;
      var submit = new Submit({
        method: 'POST',
        url: '/serialize_image',
        ajax: true,
        ajaxsuccess: function (rs) {
          var response = JSON.parse(rs);
          var logo_base64 = response.logo_base64;
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
});
