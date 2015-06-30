define(["legacy_code", "Backbone", "React", "common/button"],
  function (legacy_code, Backbone, React, Button) {

  return React.createClass({
    propTypes: {
      sigs: React.PropTypes.arrayOf(React.PropTypes.instanceOf(Signatory)).isRequired
    },

    getInitialState: function () {
      return {selectedIndex: 0};
    },

    signatoryName: function (sig) {
      return sig.smartname() != "" ? sig.smartname() : localization.pad.notNamedParty;
    },

    selected: function () {
      return this.props.sigs[this.state.selectedIndex];
    },

    handleChange: function (e) {
      this.setState({selectedIndex: e.target.value});
    },

    handleClick: function () {
      this.selected().giveForPadSigning().send();
    },

    render: function () {
      var self = this;
      var sigs = self.props.sigs;

      return (
        <div className="giveForSigningBox">
          <span className="giveForSigning">
            <span>{localization.pad.changePartyTo}</span>
            {/* if */ sigs.length > 1 &&
              <select value={self.state.selectedIndex} onChange={self.handleChange}>
                {_.map(sigs, function (sig, index) {
                  return (
                    <option key={index} value={index}>
                      {self.signatoryName(sig)}
                    </option>
                  );
                })}
              </select>
            }
            {/* else */ sigs.length <= 1 &&
              <span>
                <strong>{self.signatoryName(self.selected())}</strong>
              </span>
            }
            <Button
              size="tiny"
              type="action"
              text={localization.process.changesignatorybuttontext}
              onClick={self.handleClick}
            />
          </span>
        </div>
      );
    }
  });
});
