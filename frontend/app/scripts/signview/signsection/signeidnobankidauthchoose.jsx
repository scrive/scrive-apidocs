var Backbone = require("backbone");
var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var ViewSize = require("../viewsize");
var Button = require("../../common/button");
var classNames = require("classnames");

  module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      onBack: React.PropTypes.func.isRequired,
      onChoice: React.PropTypes.func.isRequired
    },

    getBackboneModels: function () {
      return [this.props.model];
    },

    render: function () {
      var self = this;
      var divClass = classNames({
        "col-xs-6": !ViewSize.isSmall(),
        "col-xs-12": ViewSize.isSmall(),
        "center-block": true
      });

      var document = this.props.model.document();

      var logoClass = classNames({"bankid-logo-nets": true, "no-bankid-logo": true});

      var transctionNumberText = localization.docsignview.eleg.bankid.transactionNumber;
      var transactionNumberFirst = String(document.id).slice(0, -4);
      var transactionNumberLast = String(document.id).slice(-4);
      return (
        <div className={divClass}>
          <h1 className="nets-sign-process">
            <span className={logoClass}/>
            {localization.docsignview.eleg.bankid.signNOConfirmationTitle}
          </h1>
          <p className="eid-process-view-doc-id">
            {transctionNumberText} {transactionNumberFirst}<em>{transactionNumberLast}</em>
          </p>
          <div>
            <div className="nets-sign-process">
              {localization.docsignview.eleg.bankid.signChooseMethod}
            </div>
            <Button
              text={localization.docsignview.eleg.bankid.signNOSignMethodClassic}
              onClick={function () {
                self.props.onChoice(false);
              }}
              className="button-block"
            />
            <Button
              text={localization.docsignview.eleg.bankid.signNOSignMethodMobile}
              onClick={function () {
                self.props.onChoice(true);
              }}
              className="button-block"
            />
            <Button
              text={localization.cancel}
              className="transparent-button button-block"
              onClick={this.props.onBack}
            />
          </div>
        </div>
      );
    }
  });
