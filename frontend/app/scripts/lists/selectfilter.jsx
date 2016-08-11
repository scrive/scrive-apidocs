var React = require("react");
var Select = require("../common/select");
var Track = require("../common/track");
var _ = require("underscore");


module.exports = React.createClass({
    propTypes: {
      name : React.PropTypes.string,
      options: React.PropTypes.array
    },
    render: function() {
      var self = this;
      var model = self.props.model;
      var options = this.props.options;
      var selectedValue = model.selectfiltering().filteringValue(self.props.name) || options[0].value;
      return (
        <div className='float-left'>
          <Select
            options={options}
            isOptionSelected={function (o) {
              return o.value == selectedValue;
            }}
            width={this.props.width}
            onSelect={function(value) {
                      var selectedOption = _.find(options,function(o) {return o.value == value;});
                      Track.track('Filter ' + self.props.name,
                                    {Value : selectedOption.name});
                      model.selectfiltering().setFilter(self.props.name, value);
            }}
         />
       </div>
      );
    }
});
