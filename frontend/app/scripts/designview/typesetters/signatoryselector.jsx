/** @jsx React.DOM */

define(["Backbone", "React", "common/select", "legacy_code"], function (Backbone, React, Select) {
  var SignatorySelector = React.createClass({
    propTypes: {
      field: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      className: React.PropTypes.string.isRequired,
      optionsWidth: React.PropTypes.string.isRequired,
      textWidth: React.PropTypes.number.isRequired,
      onSelect: React.PropTypes.func
    },

    handleSelect: function (s) {
      var field = this.props.field;

      mixpanel.track("Choose signature signatory");

      field.moveToSignatory(s);

      if (this.props.onSelect) {
        this.props.onSelect(s);
      }
    },

    render: function () {
      var field = this.props.field;
      var sig = field.signatory();
      var doc = sig.document();

      var signame = sig.nameOrEmail() || sig.nameInDocument();

      var options = doc.signatories().map(function (s) {
        return {name: s.nameOrEmail() || s.nameInDocument(), value: s};
      }).filter(function (s) {
        return s.name !== signame;
      });

      return (
        <div className="subtitle">
          {localization.designview.textFields.forThis + " "}
          <div className="fieldTypeSetter-subtitle-select">
            <Select.Select
              name={signame}
              options={options}
              optionsWidth={this.props.optionsWidth}
              textWidth={this.props.textWidth}
              cssClass={this.props.className}
              onSelect={this.handleSelect}
            />
          </div>
        </div>
      );
    }
  });

  return SignatorySelector;
});
