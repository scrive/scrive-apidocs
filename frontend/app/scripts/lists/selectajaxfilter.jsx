var React = require("react");
var Select = require("../common/select");
var $ = require("jquery");
var _ = require("underscore");



module.exports = React.createClass({
    propTypes: {
      name : React.PropTypes.string,
      defaultOption : React.PropTypes.object,
      optionsURL : React.PropTypes.string,
      optionsParse : React.PropTypes.func
    },
    getInitialState: function() {
      return {ajaxOptions : undefined};
    },
    getMoreDataIfNeededAndOpen : function() {
      var self = this;
      if (this.state.ajaxOptions != undefined) {
        return true;
      }
      else {
        $.ajax(
          this.props.optionsURL,
          {
            cache: false,
            success : function(res) {
              var options = self.props.optionsParse(JSON.parse(res));
              self.setState({ajaxOptions : options});
              self.open();
            }
          }
        );
        return false;
      }
    },
    open : function() {
      this.refs.select.open();
    },
    render: function() {
      var self = this;
      var model = self.props.model;
      var options = [this.props.defaultOption];
      if (this.state.ajaxOptions != undefined)
        options = options.concat(this.state.ajaxOptions);
      var selectedValue = model.selectfiltering().filteringValue(self.props.name) || options[0].value;
      var availableOptions = [];

      var selectedOptionName = "";
      for(var i=0;i< options.length;i++) {
         if (_.isEqual(options[i].value,selectedValue)) { 
           selectedOptionName = options[i].name;
         } else {
           availableOptions.push(options[i]);
         }

      }
      return (
        <div className='float-left'>
          <Select
            ref='select'
            color={"#000000"}
            options={availableOptions}
            name ={selectedOptionName}
            width={this.props.width}
            onOpen={this.getMoreDataIfNeededAndOpen}
            onSelect={function(value) {
                      var selectedOption = _.find(options,function(o) {return o.value == value;});
                      mixpanel.track('Filter ' + self.props.name,
                                    {Value : selectedOption.name});
                      model.selectfiltering().setFilter(self.props.name, value);
            }}
         />
       </div>
      );
    }
});
