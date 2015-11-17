define(["React"], function (React) {
  return React.createClass({
    render: function () {
      var overlayClass = React.addons.classSet({
        "overlay": true,
        "active": this.props.on
      });

      return (
        <div className={overlayClass} />
      );
    }
  });
});
