var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var Backbone = require("backbone");
var LanguageService = require("../../common/language_service");
var ViewSize = require("../viewsize");
var classNames = require("classnames");
var MaskedPersonalNumber = require("../identify/masked_personal_number");

module.exports = React.createClass({
    displayName: "DocViewSignatory",

    propTypes: {
      signatory: React.PropTypes.object,
      isViewingAuthor: React.PropTypes.bool,
      firstParty: React.PropTypes.bool,
      titleRow: React.PropTypes.bool
    },

    signatorySummary: function () {
      var signatory = this.props.signatory;
      var document = signatory.document();
      if (signatory.status() == "confirmationdeliveryproblem") {
        return localization.signatoryMessage.confirmationdeliveryproblem;
      } else if (signatory.signdate() != undefined && signatory.signs()) {
        return localization.signatoryMessage.signed;
      } else if (signatory.signdate() != undefined && signatory.approves()) {
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
    statusImage: function () {
      var sigStatus = this.props.signatory.status();
      if (sigStatus === "problem") {
        return "sprite-signview-problem.png";
      } else if (sigStatus === "draft") {
        return "sprite-signview-draft.png";
      } else if (sigStatus === "signed") {
        return "sprite-signview-signed.png";
      } else if (sigStatus === "approved") {
        return "sprite-signview-approved.png";
      } else if (sigStatus === "cancelled") {
        return "sprite-signview-cancelled.png";
      } else if (sigStatus === "timeouted") {
        return "sprite-signview-timeouted.png";
      } else if (sigStatus === "rejected") {
        return "sprite-signview-rejected.png";
      } else if (sigStatus === "opened") {
        return "sprite-signview-opened.png";
      } else if (sigStatus === "read") {
        return "sprite-signview-read.png";
      } else if (sigStatus === "deliveryproblem") {
        return "sprite-signview-deliveryproblem.png";
      } else if (sigStatus === "confirmationdeliveryproblem") {
        return "sprite-signview-deliveryproblem.png";
      } else if (sigStatus === "delivered") {
        return "sprite-signview-delivered.png";
      } else if (sigStatus === "sent") {
        return "sprite-signview-sent.png";
      }
    },
    censoredPersonalNumber: function () {
      const sig = this.props.signatory;
      const personalNumber = sig.personalnumber().trim();
      if (sig.current() || sig.document().currentViewerIsAuthor()) {
        return personalNumber;
      }
      const isNorwegian = (sig.noBankIDAuthenticationToView() ||
                           sig.noBankIDAuthenticationToSign() ||
                           sig.noBankIDAuthenticationToViewArchived());
      const isDanishPersonal = (
                        sig.dkNemIDCPRAuthenticationToView() ||
                        sig.dkNemIDPIDAuthenticationToView() ||
                        sig.dkNemIDCPRAuthenticationToSign() ||
                        sig.dkNemIDPIDAuthenticationToSign() ||
                        sig.dkNemIDCPRAuthenticationToViewArchived() ||
                        sig.dkNemIDPIDAuthenticationToViewArchived());
      const isDanishEmployee = sig.dkNemIDCVRAuthenticationToView()
                               || sig.dkNemIDCVRAuthenticationToViewArchived()
                               || sig.dkNemIDCVRAuthenticationToSign();
      const isFinnish = (sig.fiTupasAuthenticationToView() ||
                         sig.fiTupasAuthenticationToViewArchived());
      const mpn = new MaskedPersonalNumber({number: personalNumber,
                                            isNorwegian: isNorwegian,
                                            isDanishPersonal: isDanishPersonal,
                                            isDanishEmployee: isDanishPersonal,
                                            isFinnish: isFinnish});
      return mpn.maskNumberText();
    },
    getSsnLabel: function () {
      var sig = this.props.signatory;
      switch (sig.authenticationToSign()) {
        case "dk_nemid_cpr":
          return localization.eID.idName.cpr;
        case "dk_nemid_cvr":
          return localization.eID.idName.cvr;
        default:
          return localization.docsignview.personalNumberLabel;
      }
    },
    render: function () {
      var signatory = this.props.signatory;
      var smallView = ViewSize.isSmall();
      var mediumView = ViewSize.isMedium();

      var divClass = classNames({
        "col-xs-4": !smallView && !mediumView,
        "col-xs-6": mediumView,
        "section": smallView,
        "parties": smallView
      });

      var infoList = [];

      if (signatory.company()) {
        infoList.push({label: localization.company, text: signatory.company()});
      }

      if (signatory.email()) {
        infoList.push({label: localization.email, text: signatory.email()});
      }

      if (signatory.mobile()) {
        infoList.push({label: localization.phone, text: signatory.mobile()});
      }

      if (signatory.companynumber()) {
        infoList.push({
          label: localization.docsignview.companyNumberLabel,
          text: signatory.companynumber().trim() || localization.docsignview.notEntered
        });
      }

      if (signatory.personalnumber() && !signatory.hidePN() && !signatory.dkNemIDPIDAuthenticationToView()) {
        const info = {label: this.getSsnLabel(),
                      text: this.censoredPersonalNumber() || localization.docsignview.notEntered};
        infoList.push(info);
      }

      function renderInfo ({label, text, textClass}, index) {
        return (<li key={"info_" + index}>
                  {label}:
                  {" "}
                  <span className={textClass ? textClass : ""}>
                    <b>{text}</b>
                  </span>
                </li>);
      }

      return (
        <div className={divClass}>
          <div className={smallView ? "col-xs-12" : ""}>
            {this.props.isViewingAuthor &&
              <h1 className="title">{localization.docsignview.initiatorTitle}</h1>
            }
            {this.props.firstParty &&
              <h1 className="title">{localization.docsignview.signatoriesTitle}</h1>
            }
            <h1 className={this.props.titleRow ? "name title-margin-top" : "name"}>
              {signatory.nameOrEmailOrMobile() || localization.pad.notNamedParty}
            </h1>
            {/* if */ infoList.length < 4 &&
              <ul>
                {infoList.map(renderInfo)}
              </ul>
            }
            {/* else if */ infoList.length == 4 &&
              <span>
                <ul>
                  {infoList.slice(0, 2).map(renderInfo)}
                </ul>
                <ul>
                  {infoList.slice(2).map(renderInfo)}
                </ul>
              </span>
            }
            {/* else */ infoList.length > 4 &&
              <span>
                <ul>
                  {infoList.slice(0, 3).map(renderInfo)}
                </ul>
                <ul>
                  {infoList.slice(3).map(renderInfo)}
                </ul>
              </span>
            }
            <img
              className={"icon " + signatory.status()}
              crossOrigin="anonymous"
              src={window.cdnbaseurl + "/img/" + this.statusImage()}
            />
            <span className={"status statustext " + signatory.status()}>
              {this.signatorySummary()}
            </span>
          </div>
        </div>
      );
    }
  });
