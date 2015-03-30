define(['jquery'], function($) {

  /**
   *  @description
   *  API for form submissions to HubSpot:
   *    * Inject a form into the page by means of JSONP. We have no
   *      control over the injection procedure.
   *    * Use ugly check that the form has been mounted in the DOM,
   *      along with an iframe used for the hack that is required to
   *      circumvent page redirection upon form submission.
   *    * Fill in the form with supplied data. Hope that required fields
   *      are specified.
   *    * Submit.
   *
   *  Caveats:
   *    * Should HubSpot decide to the change class name of the div
   *      that holds this form, which is specified in
   *      `hubspotFormDomElm` below, this code will definitely
   *      *break*.
   *    * Kludgy, with extra everything.
   */


  // @note(fredrik): The id of the injected form is rather random.
  // Therefore, we use class selectors instead, and are very specific here.
  var hubspotFormDomElm = "div.hbspt-form form.hs-form";

  var hubspotIframeDomElm = "#hubspot-redirect-iframe";

  //
  // @description:
  //   Fill out form fetched by JSONP ('.hbspt-form form') with
  //   supplied JSON data and send form data to HubSpot.
  //
  var submitData = function (data, noCookie) {

    var $form = $(hubspotFormDomElm), k;

    for (k in data) {
      $form.find("input[name='" + k + "']").val(data[k]);
    }

    // Create the HubSpot context object from cookie data.
    var hutk = Cookies.get('hubspotutk');
    var hs_context = noCookie ? {} : { "hutk": hutk } ;

    // fill in an extra input field with HubSpot context object
    $form
        .prepend($('<input />')
        .attr( { "name"  : "hs_context"
               , "value" : JSON.stringify(hs_context) } ));

    $form.submit();

  };

  var mountIframe = function () {
    // mount the iframe necessary for the form redirect, if not already present.
    if ( $('#hubspot-redirect-iframe').length == 0 ) {
      $('body').append('<div id="hubspot-redirect"><iframe name="hubspot-redirect-iframe" id="hubspot-redirect-iframe"></iframe></div>');
      return true; // @devnote
    }
  };
    
  // to get vastly better errors from HubSpot, empty values are reset
  // as `undefined`.
  var checkForEmpty = function (prop) {
    var ret = ( prop === "" ) ? undefined :  prop;
    return ret;
  };

  var submitWhenReady = function (formData, noCookie){

    var hbsptIframe;
    var hbsptForm;

    // hack counter to stop injection of non-existing form.
    // No form can be injected if e.g. the correct form ids are not defined.
    var hubspotMountElmsAttempt = 0;
    var hubspotMountElmsAttemptMax = 10; // the amount of times setInterval is allowed to run.
    var hubspotMountElmsAttemptTimeDelta = 200;
    // @devnote For good, clean coding. true if mounted, false if not.
    var intervalRetval = false;

    // wait for the mount of the iframe and the form to complete.
    var intervalId = setInterval(function () {

      hbsptIframe = $('#hubspot-redirect-iframe');
      hbsptForm = $(hubspotFormDomElm);

      if ((hbsptIframe.length != 0) && (hbsptForm.length != 0) ){
        // necessary stuff is present, so submit.
        submitData(formData, noCookie);
        // ok, done so stop `setInterval`.
        intervalRetval = true;
        clearInterval(intervalId);
      }

      if (hubspotMountElmsAttempt  > hubspotMountElmsAttemptMax) {
        console.log('HubSpot iframe or form element not injected, giving up.');
        intervalRetval = false;
        clearInterval(intervalId);
      }
      else {
        hubspotMountElmsAttempt++;
      }

    }, hubspotMountElmsAttemptTimeDelta);

    return intervalRetval;

  };

var expose = {

  // shorthand for the form ids defined in the hubspotConf object
  FORM_SIGNUP         : hubspotConf.forms.signup,
  FORM_INVITE         : hubspotConf.forms.invite,
  FORM_TOS_SUBMIT     : hubspotConf.forms.tos_submit,
  FORM_NO_SENDS_DOCS  : hubspotConf.forms.no_sends_docs,
  FORM_YES_SENDS_DOCS : hubspotConf.forms.yes_sends_docs,
 
  // @description: Exposed because it might be necessary to mount the
  // form before the HubSpot submission is attempted e.g. in an AJAX
  // callback. This is done in e.g. `frontend/app/js/account_setup.js`.
  loadComponents : function (fid) {

    var hbsptIframe = $(hubspotIframeDomElm);
    var hbsptForm = $(hubspotFormDomElm);

    // check if form and iframe are mounted, and if not, attempt it.
    if (hbsptIframe.length == 0) mountIframe();

    if (hbsptForm.length == 0)
      hbspt.forms.create({
        portalId: hubspotConf.hub_id,
        formId: checkForEmpty(fid),
        onFormReady: function($form) {
          $form.attr('target', 'hubspot-redirect-iframe');
        }
      });

    // REVIEW: no need to return
    return 0; // better retval? Or just silly?

  },

  track : function(formId, formData, noCookie) {

    noCookie = (noCookie) ? true : false; // REVIEW: Are we expecting noCookie to be not boolean?

    // these might be mounted separately already
    var hbsptIframe = $(hubspotIframeDomElm);
    var hbsptForm = $(hubspotFormDomElm);

    // check if form and iframe are mounted, and if not, attempt to mount.
    if (hbsptIframe.length == 0 || hbsptForm.length == 0) expose.loadComponents(formId);

    // OK, now form should be mounted and iframe is present for the form redirection. Go.
    submitWhenReady(formData, noCookie);

    return false;
  }

}; // end expose

return expose;

});
