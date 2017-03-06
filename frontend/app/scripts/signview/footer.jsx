var React = require("react");



  module.exports = React.createClass({
    render: function() {
      return (
        <div className="section footer">
          <div className="col-xs-12">
            <img crossOrigin="anonymous" className="logo" src={window.cdnbaseurl + "/img/poweredby.png"} />
          </div>
        </div>
      );
    }
  });
