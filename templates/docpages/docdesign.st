#####
pageDocumentDesign=
$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents()$
    <script src="$homebase$/js/docsignviewcreateaccount.js"></script>
  </head>
  <body $if(custombackground)$style="background:$custombackground$;"$endif$ class='designview'>
    $header()$
    <div class="mainContainer" >
    <div class="body-container">
    <script>
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

    </script>
    </div>
    </div>
  $postPageCode()$
  </body>
$htmlclosingtag$
###
