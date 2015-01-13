/** @jsx React.DOM */

/**
 * A upload button component in React. Internallly it uses standard buttons, and can take many UI propperties of buttons - but not all of them.
 * Instead it has
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
 * var button = React.renderComponent(UploadButton({
 *    text: "ABC"
 * }), div);
 *
 */

define(['React','common/button'], function(React,Button) {
  return React.createClass({
    propTypes: {
      text        : React.PropTypes.string,
      type        : React.PropTypes.string,
      fileType    : React.PropTypes.string,
      size        : React.PropTypes.string,
      width       : React.PropTypes.number,
      className   : React.PropTypes.string,
      style       : React.PropTypes.object
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
    componentDidMount : function() {
      var self = this;
      var fileinput = $("<input class='multiFileInput' type='file'/>");
      if (self.props.fileType != "")
          fileinput.attr("accept",self.props.fileType );
      fileinput.attr("maxlength",1);
      fileinput.attr("name",self.props.name);


      fileinput.css("width",(self.refs.button.width() + 30)  + "px");
      fileinput.css("height", "66px");

      if (BrowserInfo.isIE8orLower()) {
        // make input invisible
        fileinput.css('filter', 'alpha(opacity=0)');
      }

      var list = $("<div style='display:none'/>");
      $(self.refs.button.getDOMNode()).append(list);

      fileinput.MultiFile({
        list: list,
        onError: function(a,b,c,d) {
          var splittype = model.type().split(",")[0].split("/");
          var lasttype = splittype[splittype.length - 1].toUpperCase();
          var msg = $("<span>" + localization.onlyFiletypeAllowed + "</span>");
          msg.find('.put-filetype').text(lasttype);
          new FlashMessage({content: msg, type: "error"});
          if (self.props.onError)
            self.props.onError(a,b,c,d);
        },
        afterFileAppend : function(input,title,fileinput) {
          if (self.props.onUpload != undefined) {
            self.props.onUpload(input,title,fileinput);
            fileinput.n--;
            _.each(fileinput.slaves, function(slave) {
               $(slave).remove();
            });
            fileinput.slaves = [];
            return false;
          }
        }
      });
      $(self.refs.button.getDOMNode()).append(fileinput);
    },
    render: function() {
      return (
          <Button
            text={this.props.text}
            type={this.props.type}
            size={this.props.size}
            width={this.props.width}
            className={this.props.className + " upload-button" }
            style={{position:"relative",overflow:"hidden"}}
            onClick={function() {
              return false; // Button clicks are ignored
            }}
            ref="button"
          />
      );
    }
  });
});
