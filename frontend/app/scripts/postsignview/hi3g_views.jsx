/** @jsx React.DOM */

define(['React', 'Backbone'], function(React, Backbone) {
  var expose = {};
  
  /**
   *  @description
   *  This is a standalone landing page for Hi3G iPad signing.
   *
   *  When a user have signed a document on an Hi3G iPad, which use Hi3G standalone kontrakcja system,
   *  they are redirected here. 
   *  This page is part of kontrakcja cloud version.
   */
  expose.LandingPage = React.createClass({
    propTypes: {
      email: React.PropTypes.string.isRequired,
    },

    getInitialState: function() {
      return {
	phoneNumberSubmitted: false
      };
    },

    phoneNumber: null,
    handlePhoneNumber: function(event) {
      this.phoneNumber = event.target.value;
    },
    
    analyticsRegisterUser: function(phoneNumber) {
      mixpanel.alias(this.props.email);
      mixpanel.identify(this.props.email);
      var props = {
	'Phone': this.phoneNumber,
	'Date': new Date(),
	'Hi3G lead': true,
      };
      mixpanel.register(props);
      mixpanel.people.set(props);
      mixpanel.track('Hi3G landing: Fill phonenumber');
    },

    phoneNumberSubmit: function() {
      // TODO(jens): Add error validation for phoneNumber -> Is it a phone number?
      this.analyticsRegisterUser(this.phoneNumber);
      this.setState({ 
	phoneNumberSubmitted: true
      });
    },

    render: function() {
      // The whole signed-section, is a anchor with this url. 
      //  It's by used Hi3G sellers to get back to Hi3G's system
      var backLinkUrl = "https://siw.tre.se/Scrive/paddocuments";
      
      var content_section;
      if(!this.state.phoneNumberSubmitted) {
	mixpanel.track('Hi3G landing: View');
	content_section = (
	    <div className="content-section">
	      <h1>{ localization.hi3gLandingPage.hi3gContentHeader }</h1>
	      <h2>{ localization.hi3gLandingPage.hi3gContentSubheader }</h2>
	      <div className="telephone-number">
		<input className="telephone-number-input" onChange={ this.handlePhoneNumber } type="tel"placeholder={ localization.hi3gLandingPage.hi3gPlaceholder } />
		<input className="submit" type="submit" onClick={ this.phoneNumberSubmit } value={ localization.hi3gLandingPage.hi3gSubmit }></input>
	      </div>
	    </div>
	    );	    
      } else {
	content_section = (
	    <div className="content-section">
	      <h1>{ localization.hi3gLandingPage.hi3gPhoneSubmittedTitle }</h1>
	      <h2>{ localization.hi3gLandingPage.hi3gPhoneSubmittedSubtitle }</h2>
	    </div>
	);
      }

      return (
	<div className="hi3g-landing-page">
	  <div className="inner">
	    <a href={ backLinkUrl }>
	      <div className="signed-section">
		<h1>{ localization.hi3gLandingPage.signedTitle }</h1>
		<h2>{ localization.hi3gLandingPage.signedSubtitle }</h2>
	      </div>
	    </a>
	    { content_section }
	  </div>
	</div>
      );
    }
  });
  
  return expose;
});
