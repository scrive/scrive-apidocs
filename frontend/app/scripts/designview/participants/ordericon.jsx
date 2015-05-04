/** @jsx React.DOM */

define(["legacy_code", "React"], function (_Legacy, React) {

return React.createClass({
  onClick: function () {
    var sig = this.props.model;
    mixpanel.track("Choose sign order", {
      Where: "icon"
    });
    var max = sig.document().maxPossibleSignOrder();
    var o = sig.signorder() + 1;
    if (o > max) {
      o = 1;
    }
    sig.setSignOrder(o);
  },
  render: function () {
    var self = this;
    var sig = this.props.model;
    return (
      <div
        className="design-view-action-participant-icon-order"
        onClick={function (e) {self.onClick();e.stopPropagation();}}
      >
        <div className="design-view-action-participant-icon-order-inner">
          {sig.signorder()}
        </div>
      </div>
    );
  }
});

});
