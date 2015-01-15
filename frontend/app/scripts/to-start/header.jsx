/** @jsx React.DOM */

define(['legacy_code', 'React', 'common/backbone_mixin', 'common/infotextinput', function(_legacy, React, BackboneMixin) {

return React.createClass({
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

    var status = localization.authorview.firsttime.docsent;
    if (document.closed()) status = localization.docsignview.signedAndClosed;

    return (
      <div className="instructions party-container">
        <div className="headline">{sent ? status : document.title()}</div>
      </div>
    );
  }
});

});
