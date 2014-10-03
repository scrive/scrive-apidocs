/** @jsx React.DOM */

define(['React','legacy_code'],function(React) {


return React.createClass({
    render: function() {
      return (
        <div className='float-left'>
          { this.props.children}
        </div>
      );
    }
});

});

