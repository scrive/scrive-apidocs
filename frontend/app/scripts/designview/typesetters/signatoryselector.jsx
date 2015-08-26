/** @jsx React.DOM */

define(["Backbone", "React", "common/select", "legacy_code"], function (Backbone, React, Select) {
  var SignatorySelector = React.createClass({
    propTypes: {
      field: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      onSelect: React.PropTypes.func,
      useDefaultBehavior: React.PropTypes.bool
    },

    getDefaultProps: function () {
      return {
        useDefaultBehavior: true
      };
    },

    handleSelect: function (s) {
      if (this.props.useDefaultBehavior) {
        var field = this.props.field;

        mixpanel.track("Choose signature signatory");

        field.moveToSignatory(s);
      }

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
        return s.value !== sig;
      });

      return (
        <div className="subtitle">
          {localization.designview.textFields.forThis + " "}
          <div className="fieldTypeSetter-subtitle-select">
            <Select
              name={signame}
              options={options}
              width={218}
              className="signature-field-placement-setter-field-selector"
              onSelect={this.handleSelect}
              inactive={options.length === 0}
            />
          </div>
        </div>
      );
    }
  });

  return SignatorySelector;
});
