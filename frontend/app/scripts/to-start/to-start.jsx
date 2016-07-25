var React = require("react");
var BackboneMixin = require("../common/backbone_mixin");
var Button = require("../common/button");
var ToStartSignatory = require("./signatory");
var ToStartHeader = require("./header");
var _ = require("underscore");
var FlashMessage = require("../../js/flashmessages.js").FlashMessage;
var LocalStorage = require("../../js/storage.js").LocalStorage;
var AuthorView = require("../../js/authorview/authorview.js").AuthorView;
var $ = require("jquery");


module.exports = React.createClass({
  propTypes: {
    document: React.PropTypes.object
  },
  mixins: [BackboneMixin.BackboneMixin],
  getBackboneModels: function() {
    return [this.props.document];
  },
  getInitialState: function() {
    return { 
      'sending': false
    };
  },
  orderedSignatories : function() {
    // Sort signatories so that the author ends up at the bottom of the form.
    return _.sortBy(this.props.document.signatories(), function(sig) {
      return sig.author() ? 1 : -1;
    });
  },
  finalClick: function() {
    var document = this.props.document;
    var signatories = document.signatories();
    var self = this;

    mixpanel.track('Initialized send in to-start');

    document.save();
    var probs = document.hasProblems();
    if (probs) {
      _.forEach(signatories, function(sig) {
        if (sig.hasProblems()) {
          self.refs["signatory-" + sig.id].expandToShowError();
        }
      });

      new FlashMessage({ content: localization.toStart.someFieldsNotFilledInCorrectly, cssClass: "tiny", type: 'error'});
      this.setState(this.getInitialState());

      return;
    }

    this.setState({
      'sending': true
    });

    document.afterSave(function() {
      document.makeReadyForSigning().sendAjax(function() {
        var toSignOnPad = document.signatoriesThatCanSignNowOnPad();
        if (toSignOnPad && toSignOnPad.length > 0) {
          toSignOnPad[0].giveForPadSigning().send();
          LocalStorage.set("backlink","target", "to-start");
        } else {
          // We're done, doc is sent and we want to move to author view
          window.location.reload();
        }
      });
    });
  },
  componentDidMount: function() {
    if (this.refs.authorview) {
      $('.responsive.signview').removeClass('responsive');

      var authorView = new AuthorView({id: this.props.document.id});
      var awNode = $(this.refs.authorview.getDOMNode());
      awNode.append(authorView.el());
    } else {
      this.focusOfFirstField();
    }
  },
  focusOfFirstField: function() {
    var signatory =_.find(this.orderedSignatories(), function(s) {
       return _.any(s.fields(), function(f) {
         return !f.isSignature() && !f.isCheckbox() && !f.isAuthorUnchangeableField();
       });
    });
    if (signatory && this.refs["signatory-" + signatory.id] != undefined) {
      this.refs["signatory-" + signatory.id].focusOfFirstField();
    }
  },
  render: function() {
    var document = this.props.document;
    var signatories = document.signatories();
    var documentSent = !document.preparation();

    var buttonClass = "send-button";

    if (documentSent) {
      return (<div className="authorview" ref="authorview"></div>);
    }

    return (
      <div>
        <ToStartHeader document={document} />

          {this.orderedSignatories().map(function(sig) {
            return (<ToStartSignatory key={"signatory-" + sig.id} ref={"signatory-" + sig.id} signatory={sig} />);
          })}

          <div className="party-container sign-button-container">
            <Button
              ref="sendButton"
              text={localization.toStart.sendForSigning}
              size="large"
              type="action"
              className={buttonClass}
              onClick={this.finalClick}
            />
          </div>
      </div>
    );
  }
});
