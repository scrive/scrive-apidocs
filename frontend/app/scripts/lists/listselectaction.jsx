var React = require("react");
var Select = require("../common/select");
var _ = require("underscore");


module.exports = React.createClass({
    propTypes: {
       name : React.PropTypes.string,
       actions: React.PropTypes.array
    },
    render: function() {
      var self = this;
      var model = this.props.model;
      var options = _.map(this.props.actions,function(a) {
                      return {name : a.name, onSelect : function() {a.onSelect(model); self.forceUpdate();}} ;
      });
      options.unshift({name: this.props.name, value:"", selected:true, disabled:true});
      return (
        <div className='float-right'>
          <Select
            key={Math.random()}
            options={options}
            width={this.props.width}
         />
       </div>
      );
    }
});
