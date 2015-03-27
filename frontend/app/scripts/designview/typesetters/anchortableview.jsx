/** @jsx React.DOM */

define(["Underscore", "Backbone", "React",
        "common/infotextinput", "common/button",
        "designview/typesetters/anchorrowview", "legacy_code"],
  function (_, Backbone, React, InfoTextInput, Button, AnchorRow) {
  return React.createClass({
    propTypes: {
      anchors: React.PropTypes.array.isRequired
    },

    getInitialState: function () {
      return {
        anchors: []
      };
    },

    componentWillMount: function () {
      var anchors = _.map(this.props.anchors, function (anchor) { return anchor.clone(); });

      this.setState({anchors: anchors});

      if (anchors.length === 0) {
        this.addAnchor();
      }
    },

    getAnchors: function () {
      return this.state.anchors;
    },

    addAnchor: function () {
      var newAnchors = React.addons.update(this.state.anchors, {
        $push: [new PlacementAnchor()]
      });

      this.setState({anchors: newAnchors});
    },

    removeAnchor: function (index) {
      var newAnchors = React.addons.update(this.state.anchors, {
        $splice: [[index, 1]]
      });

      this.setState({anchors: newAnchors});
    },

    render: function () {
      var self = this;

      return (
        <span>
          {self.state.anchors.length > 0 &&
            <table className="fieldTypeSetter-anchor-table">
              <thead>
                <tr>
                  <th>{localization.designview.anchorSearchPhrase}</th>
                  <th>{localization.designview.anchorIndex}</th>
                  <th />
                </tr>
              </thead>
              <tbody>
                {_.map(self.state.anchors, function (anchor, index) {
                  return (
                    <AnchorRow
                      key={index}
                      anchor={anchor}
                      onRemove={self.removeAnchor.bind(self, index)}
                    />
                  );
                })}
              </tbody>
            </table>
          }
          <Button
            ref="add"
            size="big"
            text={localization.designview.anchorAddPhrase}
            onClick={self.addAnchor}
          />
        </span>
      );
    }
  });
});
