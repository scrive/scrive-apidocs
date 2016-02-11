####
toStartLogin=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $IECompatibilityMetaTag()$
    $standardHeaderContents()$
    <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=2" />
  </head>
  <body style="background: $background$" >
  $flashmessages()$
  <div class='global-table'>
      <div class="global-table-cell">
      </div>
  </div>
  <script type="text/javascript">
   require(['jquery', 'React', 'login/login', 'legacy_code'], function(jQuery,React,Login) {
    jQuery(document).ready(function() {
      var div = jQuery('.global-table-cell');
      React.render(React.createElement(Login,{
        referer:   "/to-start",
        autofocus: true,
        nolinks : true
      }),div[0]);
      mixpanel.register({
        Context : 'Login page - to start'
      });
      mixpanel.track('View login page - to start');
    });
   });
  </script>

  </body>
$htmlclosingtag()$
###
pageToStartDocumentView=$doctype()$
$htmlopentag()$
  <head>
    <title>$documenttitle$ - $pagetitle()$</title>
    $IECompatibilityMetaTag()$
    $standardHeaderContents()$
    <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=2" />
    <link href='$cdnbaseurl$/padlist_signview_branding/$brandingdomainid$/$brandinguserid$/$brandinghash$/branding.css' rel='stylesheet' type='text/css'/>
  </head>
  <body>
    <div class="to-start-signview responsive">
      <div class="mainContainer" >
        $flashmessages()$
        <div class="to-start">
          <div class="form-container"></div>
          <script>
           require(['jquery', 'React' , 'pages/special/header', 'pages/special/footer',  'to-start/to-start', 'legacy_code'], function(jQuery,React,Header,Footer,ToStart) {

              jQuery(function(){
                doc = new Document({id: "$documentid$"});
                doc.recall(function() {
                  var toStartDiv = jQuery('<div />');
                  jQuery(".form-container").append(toStartDiv);
                  jQuery(".to-start").append(footerDiv);
                  React.render(ToStart({document: doc}), toStartDiv[0]);
                });
              });

              var headerDiv = jQuery('<div/>');
              var component = React.render(React.createElement(Header,{
                   linkText : localization.pad.backToList,
                   linkOnClick : function() {
                      window.location.href = '/to-start';
                   }
                }), headerDiv[0]);

              var footerDiv = jQuery('<div/>');
              var component = React.render(React.createElement(Footer,{
                }), footerDiv[0]);

              jQuery(".to-start").prepend(headerDiv);

            });

            mixpanel.register({
              Context : 'To Start'
            });
         </script>
        </div>
      </div>
    </div>
  </body>
$htmlclosingtag()$
####
pageToStartListView=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $IECompatibilityMetaTag()$
    $standardHeaderContents()$
    <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=2" />
    <link href='$cdnbaseurl$/padlist_signview_branding/$brandingdomainid$/$brandinguserid$/$brandinghash$/branding.css' rel='stylesheet' type='text/css'/>
  </head>
  <body>
    <div class="to-start-signview responsive">
      <div class="mainContainer" >
        $flashmessages()$
        <div class="to-start">
          <div class="list-container"></div>
          <script>
           require(['jquery', 'React' , 'pages/special/header', 'pages/special/footer', 'to-start/templatelist', 'legacy_code'], function(jQuery,React,Header,Footer, ToStartList) {

              jQuery(function(){

              var listDiv = jQuery('.list-container');
              var component = React.render(React.createElement(ToStartList,{
                }), listDiv[0]);

              var headerDiv = jQuery('<div/>');
              var component = React.render(React.createElement(Header,{
                   linkText : "$_headerLogOut()$",
                   linkOnClick : function() {
                     new Submit({
                        url : "/logout_ajax",
                        ajax : true,
                        ajaxsuccess : function() {
                          window.location.reload();}
                     }).send();
                   }
                }), headerDiv[0]);

              var footerDiv = jQuery('<div/>');
              var component = React.render(React.createElement(Footer,{
                }), footerDiv[0]);

              jQuery(".to-start").prepend(headerDiv);
              jQuery(".to-start").append(footerDiv);

            });

              mixpanel.register({
                Context : 'List for to-start'
              });

           });
         </script>
        </div>
      </div>
    </div>
  </body>
$htmlclosingtag()$
###
