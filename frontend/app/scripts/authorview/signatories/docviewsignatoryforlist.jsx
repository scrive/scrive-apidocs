var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var Backbone = require("backbone");
var classNames = require("classnames");

  module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    getBackboneModels: function () {
      return [this.props.signatory];
    },

    propTypes: {
      signatory: React.PropTypes.object
    },

    onSelect: function () {
      this.props.onSelect();
    },
    signatorySummary: function () {
      var signatory = this.props.signatory;
      var document = signatory.document();
      if (signatory.signs() && signatory.hasSigned()) {
        return localization.signatoryMessage.signed;
      } else if (signatory.approves() && signatory.hasSigned()) {
        return localization.signatoryMessage.approved;
      } else if (document.timedout() || document.canceled() || document.rejected()) {
        return localization.docsignview.unavailableForSign;
      } else if (signatory.rejecteddate() != undefined) {
        return localization.signatoryMessage.rejected;
      } else if (signatory.status() == "opened") {
        return localization.signatoryMessage.seen;
      } else if (signatory.status() == "sent" && signatory.reachedBySignorder()) {
        return localization.signatoryMessage.other;
      } else if (signatory.status() == "sent") {
        return localization.signatoryMessage.waiting;
      } else if (signatory.status() == "delivered") {
        return localization.signatoryMessage.delivered;
      } else if (signatory.status() == "read") {
        return localization.signatoryMessage.read;
      } else if (signatory.status() == "deliveryproblem") {
        return localization.signatoryMessage.deliveryproblem;
      } else {
        return localization.signatoryMessage.other;
      }
    },
    render: function () {
      var signatory = this.props.signatory;
      var divClass = classNames({
        "sig": true,
        "first": this.props.first,
        "last": this.props.last,
        "active": this.props.active
      });

      return (
        <div onClick={this.onSelect} className={divClass}>
          {/* if */ (this.props.active) &&
            <div className="arrow"/>
          }
          <div className="name">
            {signatory.nameOrEmailOrMobile()}{"\u00A0"}
          </div>
          <div className="line">
            <div className="middle">
              <div className={"icon status " + signatory.status() }> </div>
            </div>
            <div className="middle">
              <div className={"statustext " + signatory.status()}>
                  {this.signatorySummary()}
              </div>
            </div>
            <div className="middle details">
            </div>
          </div>
        </div>
      );
    }
  });
