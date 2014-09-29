/** @jsx React.DOM */

define(['legacy_code', 'React', 'common/backbone_mixin', 'common/button', 'to-start/signatory', 'to-start/header'], function(_legacy, React, BackboneMixin, Button, ToStartSignatory, ToStartHeader) {

return React.createClass({
  propTypes: {
    document: React.PropTypes.object,
    signviewbranding: React.PropTypes.object
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
  finalClick: function() {
    var document = this.props.document;
    var signatories = document.signatories();
    var self = this;

    mixpanel.track('Initialized send in to-start');

    document.save();
    var probs = document.hasProblems(true);
    if (probs) {
      _.forEach(signatories, function(sig) {
        if (sig.hasProblems(true)) {
          self.refs["signatory-" + sig.id].expandToShowError();
        }
      });

      new FlashMessage({ content: localization.toStart.someFieldsNotFilledInCorrectly, cssClass: "tiny", color: "red"});
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
  render: function() {
    var document = this.props.document;
    var signatories = document.signatories();
    var documentSent = !document.preparation();
    var signviewbranding = this.props.signviewbranding;

    var buttonClass = "send-button " + (this.state.sending ? "is-inactive" : "");

    if (documentSent) {
      var authorView = new AuthorView({
        id: document.id
        , viewer : new DocumentViewer({
          signatoryid : signatories[0].id
        })
      }); 

      this.componentDidMount = function() {
        $('.responsive.signview').removeClass('responsive');
        var awNode = $(this.refs.authorview.getDOMNode());
        awNode.empty();
        awNode.append(authorView.el);
      };

      return (<div className="authorview" ref="authorview"></div>);
    }

    // Sort signatories so that the author ends up at the bottom of the form.
    var authorLastInSignatories = _.sortBy(signatories, function(sig) {
      return sig.author() ? 1 : -1;
    });

    return (
      <div>
        <ToStartHeader document={document} />

          {authorLastInSignatories.map(function(sig) {
            return (<ToStartSignatory key={"signatory-" + sig.id} ref={"signatory-" + sig.id} signatory={sig} signviewbranding={signviewbranding} />);
          })}

          <div className="party-container sign-button-container">
            <Button 
              ref="sendButton"
              text={localization.toStart.sendForSigning}
              color="green" 
              size="large" 
              customcolor={{'background-color': signviewbranding.signviewprimarycolour(), 'color': signviewbranding.signviewprimarytextcolour()}}
              type="action"
              className={buttonClass}
              onClick={this.finalClick}
            />
          </div>
      </div>
    );
  }
});

});
