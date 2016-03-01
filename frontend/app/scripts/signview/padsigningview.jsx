import React from "react";
import Button from "../common/button";
import {Signatory} from "../../js/signatories.js";
import _ from "underscore";

module.exports = React.createClass({
  displayName: "PadSigningView",

  propTypes: {
    sigs: React.PropTypes.arrayOf(React.PropTypes.instanceOf(Signatory)).isRequired
  },

  handleClick: function (sig) {
    return function (e) {
      sig.giveForPadSigning().send();
    };
  },

  render: function () {
    var sigs = this.props.sigs;

    return (
      <div className="section padsigning">
        <div className="col-sm-6">
          <h1 className="title">{localization.pad.changePartyTo}</h1>
        </div>
        <div className="col-sm-6 right">
          {_.map(sigs, (sig, index) => {
            var name = sig.smartname() || localization.pad.notNamedParty;
            return (
              <Button
                key={index}
                type="action"
                text={name}
                onClick={this.handleClick(sig)}
              />
            );
          })}
        </div>
      </div>
    );
  }
});
