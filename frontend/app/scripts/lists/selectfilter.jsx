/** @jsx React.DOM */

define(['React','common/select','legacy_code'], function(React,NewSelect) {

return React.createClass({
    propTypes: {
      name : React.PropTypes.string,
      options: React.PropTypes.array
    },
    render: function() {
      var self = this;
      var model = self.props.model;
      var options = this.props.options;
      var selectedValue = model.selectfiltering().filteringValue(self.props.name);
      var availableOptions = [];
      var selectedOptionName = "";
      for(var i=0;i< options.length;i++) {
         if (options[i].value != selectedValue) {
           availableOptions.push(options[i]);
         } else {
           selectedOptionName = options[i].name;
         }

      }
      var Select = NewSelect.Select;
      return (
        <div className='float-left'>
          <Select
            color={"#000000"}
            options={availableOptions}
            name ={selectedOptionName}
            textWidth={this.props.textWidth}
            onSelect={function(value) {
                      var selectedOption = _.find(options,function(o) {return o.value == value;});
                      mixpanel.track('Filter ' + self.props.name,
                                    {Value : selectedOption.name});
                      model.selectfiltering().setFilter(self.props.name, value);
                      return true;
            }}
         />
       </div>
      );
    }
});

});

