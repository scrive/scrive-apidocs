/** @jsx React.DOM */

define(["Underscore", "Backbone", "React",
        "common/infotextinput", "legacy_code"],
  function (_, Backbone, React, InfoTextInput) {
  return React.createClass({
    propTypes: {
      anchor: React.PropTypes.instanceOf(PlacementAnchor).isRequired,
      onRemove: React.PropTypes.func.isRequired
    },

    restrictIndex: function (value) {
      if (!/^\d*$/.test(value)) {
        return false;
      }

      return value === "" || parseInt(value, 10) > 0;
    },

    handleBlur: function () {
      var value = this.refs.index.value();

      if (value) {
        this.props.anchor.setIndex(parseInt(value, 10));
      } else {
        this.props.anchor.setIndex(1);
        this.refs.index.setValue("1");
      }
    },

    render: function () {
      var anchor = this.props.anchor;

      return (
        <tr>
          <td className="fieldTypeSetter-anchor-text">
            <InfoTextInput
              onChange={this.props.anchor.setText}
              value={anchor.text()}
            />
          </td>
          <td className="fieldTypeSetter-anchor-index">
            <InfoTextInput
              ref="index"
              value={String(anchor.index())}
              onBlur={this.handleBlur}
              restrictInput={this.restrictIndex}
              infotext="1"
            />
          </td>
          <td className="fieldTypeSetter-anchor-delete">
            <a onClick={this.props.onRemove} />
          </td>
        </tr>
      );
    }
  });
});
