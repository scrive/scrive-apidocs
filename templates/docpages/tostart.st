####
toStartLogin=
$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents()$
    <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=2" />
  </head>
  <body style="background: $background$" >
  $flashmessages()$
  <div class='mainContainer big' style='height:auto;position: static;overflow-y:hidden;'>
      <div class="inner" style='height:auto;position:static;margin:auto;width:auto;'>
      </div>
  </div>
  <script type="text/javascript">
   require(['jquery', 'React', 'login/brandedlogin', 'legacy_code'], function(jQuery,React,BrandedLogin) {
    jQuery(document).ready(function() {
      var div = jQuery('.inner');
      React.render(React.createElement(BrandedLogin,{
        referer:   "/to-start",
        logolink:   "$if(logolink)$$logolink$$else$/img/logo_email.png$endif$",
        servicelinkcolour: $if(servicelinkcolour)$"$servicelinkcolour$"$else$null$endif$,
        textscolour: $if(textscolour)$"$textscolour$"$else$null$endif$,
        autofocus: true,
        nolinks : true
      }),div[0]);
      mixpanel.register({
        Context : 'Login page - branded'
      });
      mixpanel.track('View login page -branded');
    });
   });
  </script>

  </body>
$htmlclosingtag()$
###
pageToStartDocumentView=
$doctype()$
$htmlopentag()$
  <head>
    <title>$documenttitle$ - $pagetitle()$</title>
    $standardHeaderContents()$
    <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=2" />
    <link href='/padlist_signview_branding/$brandinghash$/branding.css' rel='stylesheet' type='text/css'/>
  </head>
  <body>
    <div class="signview responsive">
      <div class="mainContainer" >
        $flashmessages()$
        <div class="to-start">
          <div class="form-container"></div>
          <script>
           require(['jquery', 'React' , 'signview/header', 'signview/footer',  'to-start/to-start', 'legacy_code'], function(jQuery,React,Header,Footer,ToStart) {
              var svb =  new BrandingForSignView({});
              svb.fetch({ processData:  true, cache : false});

              jQuery(function(){
                doc = new Document({id: "$documentid$"});
                doc.recall(function() {
                  var toStartDiv = jQuery('<div />');
                  React.rendert(ToStart({document: doc, signviewbranding: svb}), toStartDiv[0]);
                  jQuery(".form-container").append(toStartDiv);
                  jQuery(".to-start").append(footerDiv);
                });
              });

              var headerDiv = jQuery('<div/>');
              var component = React.render(React.createElement(Header,{
                   signviewbranding: svb,
                   fullWidth: true,
                   forceShowing: true,
                   link : {
                      text : localization.pad.backToList,
                      onClick : function() {
                        window.location.href = '/to-start';
                      }
                   }
                }), headerDiv[0]);

              var footerDiv = jQuery('<div/>');
              var component = React.render(React.createElement(Footer,{
                   signviewbranding: svb,
                   forceShowing: true
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
$htmlclosingtag$
####
pageToStartListView=
$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents()$
    <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=2" />
    <link href='/padlist_signview_branding/$brandinghash$/branding.css' rel='stylesheet' type='text/css'/>
  </head>
  <body>
    <div class="signview responsive">
      <div class="mainContainer" >
        $flashmessages()$
        <div class="to-start">
          <div class="list-container"></div>
          <script>
           require(['jquery', 'React' , 'signview/header', 'signview/footer', 'to-start/templatelist', 'legacy_code'], function(jQuery,React,Header,Footer, ToStartList) {
              var svb =  new BrandingForSignView({});
              svb.fetch({ processData:  true, cache : false});

              jQuery(function(){

              var listDiv = jQuery('.list-container');
              var component = React.render(React.createElement(ToStartList,{
                   signviewbranding: svb
                }), listDiv[0]);

              var headerDiv = jQuery('<div/>');
              var component = React.render(React.createElement(Header,{
                   signviewbranding: svb,
                   fullWidth: true,
                   forceShowing: true,
                   link : {
                      text : "$_headerLogOut()$",
                      onClick : function() {
                        new Submit({
                          url : "/logout_ajax",
                          ajax : true,
                          ajaxsuccess : function() {
                            window.location.reload();}
                        }).send();
                      }
                   }
                }), headerDiv[0]);

              var footerDiv = jQuery('<div/>');
              var component = React.render(React.createElement(Footer,{
                   signviewbranding: svb,
                   forceShowing: true
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
$htmlclosingtag$
###
