var React = require("react");



module.exports = React.createClass({
    render: function() {
      return (
        <div className='float-left'>
          { this.props.children}
        </div>
      );
    }
});
