/** @jsx React.DOM */

define(['React','common/button','common/infotextinput','legacy_code'], function(React, NewButton,NewInfoTextInput) {



return React.createClass({
    render: function() {
      var self = this;
      var model = this.props.model;
      return (
        <div>
          <NewButton
            color ="black"
            text={localization.searchBoxButtonText}
            className= "float-right search-button"
            onClick = {function() {
                        mixpanel.track('Search',
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
                      mixpanel.track('Search',{
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

});

