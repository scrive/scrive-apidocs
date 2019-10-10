var React = require("react");
var Button = require("./button");
var $ = require("jquery");
var BrowserInfo = require("../../js/utils/browserinfo.js").BrowserInfo;

/**
 * A upload button component in React. Internallly it uses standard buttons, and can take many UI propperties of buttons.
 *
 * Properties:
      text        : string , text on button,  default ""
      type:       : string, "action | optional | cancel | inactive | main",
      size        : string, tiny | small | big
      width       : integer, final width of button, if not set, button will adjust to text
      className   : additional css classes
      style       : style object (react format)
      onUploadComplete : function(input, title)
      fileChecker : function(file), check every file and return bool if is ok

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
      multiple    : React.PropTypes.bool
    },
    getDefaultProps : function() {
      return {
        "className" : "",
        "text"      : "",
        "color"     : "green",
        "size"      : "small",
        "style"     : {},
        "multiple"  : false
      };
    },
    // Don't depend on this calls, since it will not work well in IE8 and IE9 (access denied on file upload)
    openFileDialogue: function () {
      if (this.isMounted()) {
        var targetElm = $(this.getDOMNode()).find("input.file-input").last();
        targetElm.click();
        return true;
      }
    },
    fileName: function(fileinput) {
      var fullPath = fileinput[0].value;
      if (fullPath) {
        var startIndex = (fullPath.indexOf('\\') >= 0 ? fullPath.lastIndexOf('\\') : fullPath.lastIndexOf('/'));
        var filename = fullPath.substring(startIndex);
        if (filename.indexOf('\\') === 0 || filename.indexOf('/') === 0) {
          filename = filename.substring(1);
        }
        return filename;
      } else {
        return "";
      }
    },
    createFileInput : function() {
      var self = this;
      if (self.isMounted()) {
        var fileinput = $("<input class='file-input' type='file' />");
        if (self.props.fileType) {
          fileinput.attr("accept", self.props.fileType);
        }

        fileinput.attr("name", self.props.name);
        fileinput.attr("multiple", self.props.multiple ? "multiple" : undefined);
        fileinput.change(function() {
          if (self.props.onUploadComplete != undefined) {
            // IE8 requires delay before inputs are available.
            setTimeout(function () {
              var filesOK = _.every(fileinput[0].files, function (file) {
                if (self.props.fileChecker !== undefined) {
                  return self.props.fileChecker(file);
                } else {
                  return true;
                }
              });
              if (filesOK) {
                fileinput.detach();
                self.props.onUploadComplete(fileinput, self.fileName(fileinput));
                self.createFileInput();
              } else {
                fileinput.remove();
                self.createFileInput();
              }
            }, 100);
          } else {
            fileinput.remove();
            self.createFileInput();
          }
        });
        $(this.getDOMNode()).prepend(fileinput);
      }
    },
    componentDidMount : function() {
      this.createFileInput();
    },
    render: function() {
      var self = this;
      return (
        <div
          className={"upload-button " + this.props.className}
          style={this.props.style}
          >
          <Button
            text={this.props.text}
            type={this.props.type}
            size={this.props.size}
            width={this.props.width}
            onClick={function() {
              // do nothing. It should never happend since users should click on transparent file input
            }}
          />
        </div>
      );
    }
  });
