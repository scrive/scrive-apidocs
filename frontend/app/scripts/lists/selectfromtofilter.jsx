/** @jsx React.DOM */

define(['React', 'common/select','legacy_code'], function(React, Select) {

return React.createClass({
    propTypes: {
      nameFrom : React.PropTypes.string,
      fromText : React.PropTypes.string,
      toText : React.PropTypes.string,
      options : React.PropTypes.array
    },
    render: function() {
      var self = this;
      var model = self.props.model;
      var options = self.props.options;
      var fromValue = model.selectfiltering().filteringValue("from_"+self.props.name);
      var toValue = model.selectfiltering().filteringValue("to_" + self.props.name);
      var currentFromOption = _.find(options,function(o) { return _.isEqual(o.fromValue,fromValue);});
      var currentToOption = _.find(options,function(o) { return _.isEqual(o.toValue,toValue);});
      var currentFromName = currentFromOption != undefined ? this.props.fromText + " " + currentFromOption.name : this.props.fromText;
      var currentToName = currentToOption != undefined ? this.props.toText + " " + currentToOption.name : this.props.toText;

      var optionsFrom = [];
      for(var i=0;i<options.length;i++)
      {
        optionsFrom.push({ name: options[i].name, value: options[i].fromValue });
        if (_.isEqual(options[i].toValue,toValue))
          break; // Stop adding, else limits can overlap
      }
      optionsFrom.reverse();

      if (!_.isEqual(fromValue,this.props.emptyValue)) {
        optionsFrom.unshift({name : this.props.fromText, value: this.props.emptyValue});
      }

      var optionsTo = [];
      for(var i=options.length - 1 ;i>=0;i--)
      {
        optionsTo.push({ name: options[i].name, value: options[i].toValue });
        if (_.isEqual(options[i].fromValue,fromValue))
          break;  // Stop adding, else limits can overlap
      }
      if (!_.isEqual(toValue,this.props.emptyValue))
        optionsTo.unshift({name : this.props.toText, value: this.props.emptyValue});

      return (
        <div className='float-left'>
          <div className='float-left'>
            <Select
              color={"#000000"}
              options={optionsFrom}
              name ={currentFromName}
              width={this.props.width}
              onSelect={function(value) {
                        model.selectfiltering().setFilter("from_" + self.props.name, value);
              }}
          />
         </div>
         <div className='float-left'>
          <Select
              color={"#000000"}
              options={optionsTo}
              name ={currentToName}
              width={this.props.width}
              onSelect={function(value) {
                        model.selectfiltering().setFilter("to_" + self.props.name, value);
              }}
          />
        </div>
       </div>
      );
    }
});

});

