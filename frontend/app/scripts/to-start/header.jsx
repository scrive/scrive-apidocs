var React = require("react");
var BackboneMixin = require("../common/backbone_mixin");


module.exports = React.createClass({
  propTypes: {
    document: React.PropTypes.object
  },
  mixins: [BackboneMixin.BackboneMixin],
  getBackboneModels: function() {
    return [this.props.document];
  },
  render: function() {
    var document = this.props.document;
    var sent = !document.preparation();

    var status = localization.toStart.docSent;
    if (document.closed()) status = localization.docsignview.signedAndClosed;

    return (
      <div className="instructions party-container">
        <div className="headline">{sent ? status : document.title()}</div>
      </div>
    );
  }
});
