/** @jsx React.DOM */

define(['React','legacy_code'],function(React) {

return React.createClass({
    render: function() {
      return (
        <div className=''>
          { this.props.children }
       </div>
      );
    }
});

});

