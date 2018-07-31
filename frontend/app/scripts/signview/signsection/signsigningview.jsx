var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var Button = require("../../common/button");
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");
var ViewSize = require("../viewsize");
var $ = require("jquery");
var classNames = require("classnames");
var SignHeader = require("./signheader");
var SignLegalAgreement = require("./signlegalagreement");

  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      title: React.PropTypes.string.isRequired,
      name: React.PropTypes.string.isRequired,
      canSign: React.PropTypes.bool.isRequired,
      onBack: React.PropTypes.func.isRequired,
      onSign: React.PropTypes.func.isRequired
    },

    render: function () {
      var hasSignaturesPlaced = this.props.model.document().currentSignatory().hasPlacedSignatures();

      var divClass = classNames({
        "col-xs-6": !ViewSize.isSmall(),
        "col-xs-12": ViewSize.isSmall(),
        "center-block": true
      });

      return (
        <div className={divClass}>
          {/* if */ hasSignaturesPlaced &&
            <div>
              <h1>{localization.process.signModalTitle}</h1>
              <SignLegalAgreement />
              <p>
                <HtmlTextWithSubstitution
                  secureText={hasSignaturesPlaced ? localization.signviewConfirmationSignaturesPlaced :
                                                    localization.signviewConfirmation}
                  subs={{".put-document-title-here": this.props.title, ".put-signatory-name-here": this.props.name}}
                />
              </p>
            </div>
          }
          {/* else */ !hasSignaturesPlaced &&
            <SignHeader title={this.props.title} name={this.props.name} />
          }
          <Button
            type="action"
            ref="signButton"
            className="button-block sign-button"
            onClick={this.props.onSign}
            text={hasSignaturesPlaced ? localization.process.signbuttontextfromsignaturedrawing :
                                        localization.process.signbuttontext}
          />
          <Button
            className="transparent-button button-block"
            onClick={this.props.onBack}
            text={localization.toStart.backFromSigningPage}
          />
        </div>
      );
    }
  });
