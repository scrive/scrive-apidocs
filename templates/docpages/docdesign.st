#####
pageDocumentDesign=
$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents()$
    <script>
     require(['Backbone', 'legacy_code'], function(Backbone) {
       // need Backbone to be definied before we load this file
       require(['$homebase$/js/docsignviewcreateaccount.js'], function() {});
     });
    </script>
  </head>
  <body $if(custombackground)$style="background:$custombackground$;"$endif$ class='designview'>
    $header()$
    $flashmessages()$

    <div class="mainContainer" >
    <div class="body-container">
    <script>
     require(['jquery', 'legacy_code'], function(jQuery) {
    jQuery(function(){
      var dv = new DesignView({
                            id:"$documentid$"
                      });

      mixpanel.register({DocumentID: "$documentid$", Context: "design view"});
      mixpanel.track('View Design View');

      jQuery(".body-container").replaceWith(
        jQuery('<div />').append(BlockingInfo.el())
                        .append(dv.el())
                        .children()
      );
      dv.afterInsert();

      if (FirstTimeUserExperience.isActive()) {
        FirstTimeUserExperience.reset();
        new DesignViewTour({
            model: dv.model()
        })
      }
    });
     });

    </script>
    </div>
    </div>
  $postPageCode()$
  </body>
$htmlclosingtag$
###
