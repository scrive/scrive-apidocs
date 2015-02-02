/** @jsx React.DOM */

define(["React","legacy_code","common/select"], function(React, _Legacy, NewSelect) {

return React.createClass({
  propTypes: {
    getFont: React.PropTypes.func,
    setFont: React.PropTypes.func,
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
    var Select  = NewSelect.Select;
    var availableFonts = self.availableFonts();
    var availableFontOptions = [];
    var selectedFontName = localization.branding.themes.customFont;
    var selectedFontValue = "";

    _.each(availableFonts,function(f) {
      if (f.value == self.props.getFont()) {
        selectedFontName = f.name;
        selectedFontValue = f.value;
      } else {
        availableFontOptions.push({
          name : f.name,
          style: {fontFamily : f.value},
          onSelect : function() {
            self.props.setFont(f.value);
            return false;
          }
        });
      }
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
            dangerouslySetInnerHTML={{__html: this.props.sampleText}}
            >
          </div>
          <div className="theme-edit-font-select">
              <Select
                color={"#000000"}
                options={availableFontOptions}
                name ={selectedFontName}
                textWidth = {109}
                optionsWidth = "136px"
                style={{fontFamily : selectedFontValue}}
              />
          </div>
        </div>
      </div>
    );
  }
});

});
