define(["legacy_code", "Backbone", "React", "common/button"],
  function (legacy_code, Backbone, React, Button) {

  return React.createClass({
    propTypes: {
      sigs: React.PropTypes.arrayOf(React.PropTypes.instanceOf(Signatory)).isRequired
    },

    getInitialState: function () {
      return {selectedIndex: 0};
    },

    selected: function () {
      return this.props.sigs[this.state.selectedIndex];
    },

    handleChange: function (e) {
      this.setState({selectedIndex: e.target.value});
    },

    handleClick: function (sig) {
      return function (e) {
        sig.giveForPadSigning().send();
      };
    },

    render: function () {
      var self = this;
      var sigs = self.props.sigs;

      return (
        <div className="give-for-signing">
          <div className="change-party-text">{localization.pad.changePartyTo}</div>
          {_.map(sigs, function (sig, index) {
            var name = sig.smartname() || localization.pad.notNamedParty;
            return (
              <Button
                key={index}
                type="action"
                text={name}
                onClick={self.handleClick(sig)}
              />
            );
          })}
        </div>
      );
    }
  });
});
