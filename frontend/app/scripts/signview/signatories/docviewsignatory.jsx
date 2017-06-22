var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var Backbone = require("backbone");
var LanguageService = require("../../common/language_service");
var ViewSize = require("../viewsize");
var classNames = require("classnames");

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

      if (signatory.personalnumber()) {
        infoList.push({
          label: localization.docsignview.personalNumberLabel,
          text: signatory.personalnumber().trim() || localization.docsignview.notEntered
        });
      }

      function renderInfo ({label, text}, index) {
        return <li key={"info_" + index}>{label}: <b>{text}</b></li>;
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
            <span className={"icon status " + signatory.status()}></span>
            <span className={"status statustext " + signatory.status()}>
              {this.signatorySummary()}
            </span>
          </div>
        </div>
      );
    }
  });
