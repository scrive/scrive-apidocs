/** @jsx React.DOM */

define(['React', 'common/select','legacy_code'], function(React, NewSelect) {

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
      var currentFromOption = _.find(options,function(o) { return o.value == fromValue});
      var currentToOption = _.find(options,function(o) { return o.value == toValue});
      var currentFromName = currentFromOption != undefined ? this.props.fromText + " " + currentFromOption.name : this.props.fromText;
      var currentToName = currentToOption != undefined ? this.props.toText + " " + currentToOption.name : this.props.toText;

      var optionsFrom = [];
      for(var i=0;i<options.length;i++)
      {
        optionsFrom.push(_.clone(options[i]));
        if (options[i].value == toValue)
          break; // Stop adding, else limits can overlap
      }
      optionsFrom.reverse();

      if (fromValue != "")
        optionsFrom.unshift({name : this.props.fromText, value: ""});


      var optionsTo = [];
      for(var i=options.length - 1 ;i>=0;i--)
      {
        optionsTo.push(_.clone(options[i]));
        if (options[i].value == fromValue)
          break;  // Stop adding, else limits can overlap
      }
      if (toValue != "")
        optionsTo.unshift({name : this.props.toText, value: ""});

      var Select = NewSelect.Select;
      return (
        <div className='float-left'>
          <div className='float-left'>
            <Select
              color={"#000000"}
              options={optionsFrom}
              name ={currentFromName}
              textWidth={this.props.textWidth}
              onSelect={function(value) {
                        model.selectfiltering().setFilter("from_" + self.props.name, value);
                        return true;
              }}
          />
         </div>
         <div className='float-left'>
          <Select
              color={"#000000"}
              options={optionsTo}
              name ={currentToName}
              textWidth={this.props.textWidth}
              onSelect={function(value) {
                        model.selectfiltering().setFilter("to_" + self.props.name, value);
                        return true;
              }}
          />
        </div>
       </div>
      );
    }
});

});

