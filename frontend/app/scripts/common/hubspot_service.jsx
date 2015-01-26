define(['jquery'], function($) {

  // @note(fredrik): The id of the injected form is rather random.
  // Therefore, we are very specific here.
  var hubspotFormId = "div.hbspt-form form.hs-form";

  //
  // @description:
  //   Fill out form fetched by JSONP ('.hbspt-form form') with
  //   supplied JSON data and send form data to HubSpot.
  //
  var submitData = function (data) {

    var $form = $(hubspotFormId), k;

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
      formId: fid,
      onFormReady: function($form) {
        $form.attr('target', 'hubspot-redirect-iframe');
      }
    });

    return hbsptForm;

  };

var expose = {

  // shorthand for the form ids defined in the hubspotConf object
  FORM_SIGNUP         : hubspotConf.forms.signup,
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

    // begin fetching the HubSpot form. Wait for this below as well...
    loadForm(formId);

    // wait for the mount of the iframe to complete and the form to be
    // loaded, and only then submit the data.
    var intervalId = setInterval(function () {
      hbsptIframe = $('#hubspot-redirect-iframe');
      hbsptForm = $(hubspotFormId);
      if ((hbsptIframe.length != 0) && (hbsptForm.length != 0) ){
         submitData(formData);
         clearInterval(intervalId);
      } 
    }, 150);

    return false;
  }

}; // end expose

return expose;

});
