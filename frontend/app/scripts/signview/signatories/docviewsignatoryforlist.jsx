/** @jsx React.DOM */

define(["React", "common/backbone_mixin", "Backbone", "legacy_code"],
  function (React, BackboneMixin, Backbone) {

  return React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    getBackboneModels: function () {
      return [this.props.signatory];
    },

    propTypes: {
      signatory: React.PropTypes.object
    },

    signatorySummary: function () {
      var signatory = this.props.signatory;
      var document = signatory.document();
      if (signatory.signdate() != undefined) {
        return localization.signatoryMessage.signed;
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
      } else if (localization.signatoryMessage[signatory.status()] != undefined) {
        return localization.signatoryMessage[signatory.status()];
      } else {
        return localization.signatoryMessage.other;
      }
    },

    onSelect: function () {
      this.props.onSelect();
    },

    render: function () {
      var signatory = this.props.signatory;
      var divClass = React.addons.classSet({
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
              <div className={"icon status " + signatory.status()}> </div>
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
});
