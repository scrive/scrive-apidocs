var React = require("react");
var Track = require("../../common/track");

module.exports = React.createClass({
  onClick: function () {
    var sig = this.props.model;
    Track.track("Choose sign order", {
      Where: "icon"
    });
    var max = sig.document().maxPossibleSignOrder();
    var o = sig.signorder() + 1;
    if (o > max) {
      o = 1;
    }
    sig.setSignOrder(o);
  },
  title: function () {
    var title = [
      localization.designview.addParties.invitationOrder,
      "" + this.props.model.signorder()
    ];

    return title.join(": ");
  },
  render: function () {
    var self = this;
    var sig = this.props.model;
    return (
      <div
        className="design-view-action-participant-icon-order"
        onClick={function (e) { self.onClick(); e.stopPropagation(); }}
        title={this.title()}
      >
        <div className="design-view-action-participant-icon-order-inner">
          {sig.signorder()}
        </div>
      </div>
    );
  }
});
