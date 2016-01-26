####
pagePadListView=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents(nobranding='true')$
    <link href='$cdnbaseurl$/padlist_signview_branding/$brandingdomainid$/$brandinguserid$/$brandinghash$/branding.css' rel='stylesheet' type='text/css'/>
  </head>
  <body>
    <div class="to-start-signview">
      <div class="mainContainer" >
        $flashmessages()$
        <div class="pad-list">
          <div class="list-container"></div>
          <script>
           require(['jquery', 'React' , 'pages/special/header', 'pages/special/footer', 'padlist/padlist',  'legacy_code'], function(jQuery,React,Header,Footer,PadList) {

             jQuery(function(){
              var padlistDiv = jQuery('<div/>')
              var padlist = React.render(React.createElement(PadList,{}),padlistDiv[0]);
              var refresherPadListStep = function() {
                setTimeout(function() {
                  padlist.refreshStep();
                  refresherPadListStep();
                },1000);
              };
              refresherPadListStep();
              var headerDiv = jQuery('<div/>');
              React.render(React.createElement(Header,{
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
              React.render(React.createElement(Footer,{
                }), footerDiv[0]);
              jQuery(".pad-list").prepend(headerDiv);
              jQuery(".list-container").append(padlistDiv);
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
$htmlclosingtag()$
####
