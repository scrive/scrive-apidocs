var React = require("react");
var Select = require("../common/select");
var _ = require("underscore");


module.exports = React.createClass({
  propTypes: {
    getFont: React.PropTypes.func,
    setFont: React.PropTypes.func
  },
   availableFonts: function() {
     return [
         {name :'Comic Sans MS', value: '\"comic sans ms\",sans-serif'}
       , {name :'Courier New', value: '\"courier new\",monospace'}
       , {name :'Helvetica', value: 'helvetica,sans-serif'}
       , {name :'Garamond', value:  'garamond,serif'}
       , {name :'Georgia', value:  'georgia,serif'}
       , {name :'Narrow', value:  '\"arial narrow\",sans-serif'}
       , {name :'Sans Serif', value:  'arial,helvetica,sans-serif'}
       , {name :'Serif', value:  '\"times new roman\",serif'}
       , {name :'Source Sans Pro', value: '\"Source Sans Pro\", \"Helvetica Neue\", Arial, sans-serif'}
       , {name :'Tahoma', value: 'tahoma,sans-serif'}
       , {name :'Trebuchet MS', value:  '\"trebuchet ms\",sans-serif'}
       , {name :'Verdana', value:  'verdana,sans-serif'}
       , {name :'Wide', value: '\"arial black\",sans-serif'}
     ];
   } ,
  render: function() {
    var self = this;
    var availableFonts = self.availableFonts();
    var availableFontOptions = [];
    var selectedFontValue = "";

    _.each(availableFonts,function(f) {
      if (f.value == self.props.getFont()) {
        selectedFontValue = f.value;
      }
      availableFontOptions.push({
        name : f.name,
        selected : f.value == self.props.getFont(),
        onSelect : function() {
          self.props.setFont(f.value);
          return false;
        }
      });
    });

    return (
      <div className="theme-font-editor">
        <div className="title theme-font-editor-title">
          {this.props.title}
        </div>
        <div className="theme-font-editor-edit">
          <div
            key={Math.random()} // This is needed, else react will not regenerate styles
            className="font-sample"
            style={{fontFamily: this.props.getFont()}}
            >
            {this.props.sampleText}
          </div>
          <div className="theme-edit-font-select">
              <Select
                options={availableFontOptions}
                width = {136}
                style={{fontFamily : selectedFontValue}}
              />
          </div>
        </div>
      </div>
    );
  }
});
