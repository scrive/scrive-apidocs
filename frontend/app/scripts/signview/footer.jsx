/** @jsx React.DOM */


define(['React'], function(React) {

  return React.createClass({
    render: function() {
      return (
        <div className="section footer">
          <div className="col-xs-12 center">
            <img className="logo" src={window.cdnbaseurl + "/img/poweredby.png"} />
          </div>
        </div>
      );
    }
  });

});
