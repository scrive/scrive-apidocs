/** @jsx React.DOM */

define(['React','common/select','legacy_code'], function(React,NewSelect) {

return React.createClass({
    propTypes: {
       name : React.PropTypes.string,
       actions: React.PropTypes.array
    },
    render: function() {
      var model = this.props.model;
      var Select = NewSelect.Select;
      var options = _.map(this.props.actions,function(a) {
                      return {name : a.name, onSelect : function() {a.onSelect(model); }} ;
      });
      return (
        <div className='float-right'>
          <Select
            color={"#000000"}
            options={options}
            name ={this.props.name}
            optionsWidth={this.props.optionsWidth}
            textWidth={this.props.textWidth}
         />
       </div>
      );
    }
});

});

