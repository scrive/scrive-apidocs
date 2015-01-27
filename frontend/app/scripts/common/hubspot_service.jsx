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

  //
  // @description:
  //   Fill out form fetched by JSONP ('.hbspt-form form') with
  //   supplied JSON data and send form data to HubSpot.
  //
  var submitData = function (data) {

    var $form = $(hubspotFormDomElm), k;

    for (k in data) {
      $form.find("input[name='" + k + "']").val(data[k]);
    }

    // Create the HubSpot context object from cookie data.
    var hutk = Cookies.get('hubspotutk');
    var hs_context = { "hutk": hutk };

    // fill in an extra input field with HubSpot context object
    $form
        .prepend($('<input />')
        .attr( { "name"  : "hs_context"
               , "value" : JSON.stringify(hs_context) } ));

    $form.submit();

  };

  var loadForm = function (fid) {

    var hbsptForm = hbspt.forms.create({
      portalId: hubspotConf.hub_id,
      formId: checkForEmpty(fid),
      onFormReady: function($form) {
        $form.attr('target', 'hubspot-redirect-iframe');
      }
    });

    return hbsptForm;

  };

  // to get vastly better errors from HubSpot, empty values are reset
  // as `undefined`.
  var checkForEmpty = function (prop) {
    var ret = ( prop === "" ) ? undefined :  prop;
    return ret;
  }

var expose = {

  // shorthand for the form ids defined in the hubspotConf object
  // REVIEW: Fixa kommenteringen :)
//  FORM_SIGNUP         : hubspotConf.forms.signup,
  FORM_SIGNUP         : "",
  FORM_INVITE         : hubspotConf.forms.invite,
  FORM_TOS_SUBMIT     : hubspotConf.forms.tos_submit,
  FORM_NO_SENDS_DOCS  : hubspotConf.forms.no_sends_docs,
  FORM_YES_SENDS_DOCS : hubspotConf.forms.yes_sends_docs,
 
  track : function(formId, formData) {

    var hbsptIframe; // used when waiting for node mount below
    var hbsptForm; // fetched remotely by JSONP.

    // mount the iframe necessary for the form redirect, if not already present.
    if ( $('#hubspot-redirect-iframe').length == 0 ) {
       $('body').append('<div id="hubspot-redirect"><iframe name="hubspot-redirect-iframe" id="hubspot-redirect-iframe"></iframe></div>');
    }

    // try fetching the HubSpot form. Wait for this below.
    loadForm(formId);

    // hack counter to stop injection of non-existing form.
    // No form can be injected if e.g. the correct form ids are not defined.
    var hubspotCallCounter = 0;
    var hubspotCallMax     = 100; // change // REVIEW: change nu eller sen? ta bort kommentaren om sen.

    // wait for the mount of the iframe to complete and the form to be
    // loaded, and only then submit the data.
    var intervalId = setInterval(function () {
      hbsptIframe = $('#hubspot-redirect-iframe');
      hbsptForm = $(hubspotFormDomElm);
      
      if ((hbsptIframe.length != 0) && (hbsptForm.length != 0) ){
         submitData(formData);
         clearInterval(intervalId);
      }

      if (hubspotCallCounter > hubspotCallMax) {
        // stop trying  -- either the frame can't be injected,
        // or the form id is not recognised by HubSpot.
        // REVIEW: Gör en console.log här
        clearInterval(intervalId);
      }
      else {
        hubspotCallCounter++;
      }

    }, 150);

    return false;
  }

}; // end expose

return expose;

});
