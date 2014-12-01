/** @jsx React.DOM */

define(['React', 'common/button', 'eleg/bankid', 'Backbone', 'legacy_code'], function(React, Button, BankID, Backbone) {

// TODO add some useful mixpanel events
var SignWithElegModalButtons = React.createClass({
  propTypes : {
    signatory : React.PropTypes.object,
    callback  : React.PropTypes.func,
    errorcallback : React.PropTypes.func,
    modal : React.PropTypes.object
  },
  buttonOnClick : function(thisDevice) {
    var self = this;
    self.props.modal.close();
    BankID({
      signatory : self.props.signatory,
      callback  : self.props.callback,
      errorcallback  : self.props.errorcallback,
      thisDevice : thisDevice
    });
  },
  render : function() {
    var self = this;
    return (
      <div>
        <Button text={localization.docsignview.eleg.bankid.modalThisDevice}
                color="grey"
                className="bankid"
                onClick={function() {
                  self.buttonOnClick(true);
                }}
        />
        <Button text={localization.docsignview.eleg.bankid.modalAnotherDevice}
                color="grey"
                className="bankid"
                onClick={function() {
                  self.buttonOnClick(false);
                }}
        />
      </div>
    );
  }
});


return function(args) {
    var self = this;
    var buttons = $('<div>');
    // TODO style things properly for small screens
    self.modal = new Confirmation({
      title : localization.docsignview.eleg.chooseElegModalTitle,
      cssClass: 'grey sign-eleg-option-modal' + (BrowserInfo.isSmallScreen() ? ' small-device' : ''),
      content : localization.docsignview.eleg.chooseElegModalContent,
      acceptButton : buttons
    });

    React.renderComponent(SignWithElegModalButtons({
      signatory : args.signatory,
      callback  : args.callback,
      errorcallback  : args.errorcallback,
      modal : self.modal
    }), buttons[0]);
};

});
