var Backbone = require("backbone");
var React = require("react");
var Select = require("../../common/select");
var Track = require("../../common/track");

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

        Track.track("Choose signature signatory");

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

      var options = doc.signatories().map(function (s) {
        return {
          name: s.nameOrEmail() || s.nameInDocument(),
          value: s,
          selected: s === sig
        };
      });

      return (
        <div className="subtitle">
          {localization.designview.textFields.forThis + " "}
          <div className="fieldTypeSetter-subtitle-select">
            <Select
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

  module.exports = SignatorySelector;
