define(['jquery'], function($) {

var expose = {

  // @note(fredrik): The id of the injected form is rather random.
  // Therefore, I try to be very specific here. 
  hubspotFormId : "div.hbspt-form form.hs-form",

  submitData : function (data) {

    // Fill out form fetched by JSONP ('.hbspt-form form') with
    // supplied JSON data and send form data to HubSpot.

    var $form = $(this.hubspotFormId), k;
    // @todo(fredrik) Add support for checkbox/radio.
    for (k in data) {
      $form.find("input[name='" + k + "']").val(data[k]);
    }
    $form.submit();
  },
 
  track : function(formId, formData) {
    //
    // @note(fredrik)
    //   * move body append? YES!
    //   * check if iframe exists first....
    //

    var hbsptIframe; // used when waiting for node mount below
    var hbsptForm; // fetched remotely by JSONP.
    var hbsptSelf = this;

    // mount the iframe necessary for the form redirect
    // @note(fredrik) remove 'no' in style attribute.
    $('body').append('<div id="hubspot-redirect" style="nodisplay:none;novisibility:hidden;"><iframe name="hubspot-redirect-iframe" id="hubspot-redirect-iframe"></iframe></div>');

    // begin fetching the HubSpot form. Wait for this below as well...
    hbsptSelf.loadForm(formId);

    // wait for the mount to complete and the form to be loaded, and only then
    // submit the data.
    var intervalId = setInterval(function () {
      hbsptIframe = $('#hubspot-redirect-iframe');
      hbsptForm = $(hbsptSelf.hubspotFormId);
      if ((hbsptIframe.length != 0) && (hbsptForm.length != 0) ){
         hbsptSelf.submitData(formData);
         clearInterval(intervalId);
      } 
    }, 150);

    return false;
  },

  loadForm : function (fid) {

    //
    // @todo(fredrik) : check if already present first
    //
    // @note(fredrik) 
    //
    // Create the form by way of `window.hbspt` and insert into into
    // the DOM.
    //

    var hbsptForm = hbspt.forms.create({
      portalId: hubspotConf.hub_id,
      formId: fid,
      onFormReady: function($form) {
        $form.attr('target', 'hubspot-redirect-iframe');
      }
    });

    return hbsptForm;

  }
}; // end expose

return expose;

});
