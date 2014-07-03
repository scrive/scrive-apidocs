###
pagePadListView=
$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents()$
  </head>
  <body>
    <div class="signview">
      <div class="mainContainer" >
        $flashmessages()$
        <div class="pad-list">
          <div class="list-container"></div>
          <script>
           require(['jquery', 'React' , 'signview/header', 'signview/footer',  'legacy_code'], function(jQuery,React,Header,Footer) {
              var svb =  new BrandingForSignView({});
              svb.fetch({ processData:  true, cache : false});

             jQuery(function(){
              var padlist = new PadList({});

              var headerDiv = jQuery('<div/>');
              var component = React.renderComponent(
                Header({
                   signviewbranding: svb,
                   fullWidth: true,
                   link : {
                      text : "$_headerLogOut()$",
                      onClick : function() {
                        new Submit({
                          url : "/logout",
                          ajax : true,
                          ajaxsuccess : function() {
                            window.location.reload();}
                        }).send();
                      }
                   }
                }), headerDiv[0]);

              var footerDiv = jQuery('<div/>');
              var component = React.renderComponent(
                Footer({
                   signviewbranding: svb
                }), footerDiv[0]);
              jQuery(".pad-list").prepend(headerDiv);
              jQuery(".list-container").append(padlist.el());
              jQuery(".pad-list").append(footerDiv);

            });

              mixpanel.register({
                Context : 'List for pad signing'
              });

           });
         </script>
        </div>
      </div>
    </div>
  </body>
$htmlclosingtag$
####