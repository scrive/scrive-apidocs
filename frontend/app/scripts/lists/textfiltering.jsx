var React = require("react");
var NewButton = require("../common/button");
var NewInfoTextInput = require("../common/infotextinput");
var Track = require("../common/track");



module.exports = React.createClass({
    render: function() {
      var self = this;
      var model = this.props.model;
      return (
        <div>
          <NewButton
            text={localization.searchBoxButtonText}
            className= "float-right search-button"
            onClick = {function() {
                        Track.track('Search',
                                        {Query : self.refs.searchinput.value(),
                                        Input : "Button"});
                        model.setText(self.refs.searchinput.value());
                      }}
          />
          <NewInfoTextInput
            infotext={this.props.text}
            value={model.text()}
            className="list-search float-right"
            ref="searchinput"
            onEnter={function() {
                      Track.track('Search',{
                                      Input : "Enter",
                                      Query : self.refs.searchinput.value()
                                    });
                      model.setText(self.refs.searchinput.value());
                    }}
          />
        </div>
      );
    }
});
