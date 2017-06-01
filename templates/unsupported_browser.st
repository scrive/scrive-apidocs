unsupportedBrowser=$doctype()$
$htmlopentag()$
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    <title>$_unsupportedBrowserTitle()$</title>
    <meta http-equiv="content-language" content="en" />
    <style media="screen" type="text/css">
      @font-face {
        font-family: "Helvetica Neue";
        src: url("/fonts/HelveticaNeue.eot");
      }
    </style>

    <script type="text/javascript">
      var browser = navigator.userAgent;
    </script>

    <!--[if lte IE 10]>
      <script type="text/javascript">
        browser = 'Internet Explorer 10';
      </script>
    <![endif]-->

    <!--[if lte IE 9]>
      <script type="text/javascript">
        browser = 'Internet Explorer 9';
      </script>
    <![endif]-->

    <!--[if lte IE 8]>
      <script type="text/javascript">
        browser = 'Internet Explorer 8';
      </script>
    <![endif]-->

    <!--[if lte IE 7]>
      <script type="text/javascript">
        browser = 'Internet Explorer 7';
      </script>
    <![endif]-->

    <!--[if lte IE 6]>
      <script type="text/javascript">
        browser = 'Internet Explorer 6';
      </script>
    <![endif]-->

    <script type="text/javascript">
      window.onload = function() {
        var browser_name_div = document.getElementById('browser_name');
        browser_name_div.appendChild(browser_name_div.ownerDocument.createTextNode(browser));
      };
    </script>

  </head>
  <body style="background-color: #F7F7F7; font-family: 'Helvetica Neue';">

    <div style="width: 100%; min-height: 95px; border-top: 1px solid #dee4ed; border-bottom: 1px solid #dee4ed; margin-top: 3px; background-color: #FFFFFF;">
      <div style="width: 960px; margin: 0 auto; vertical-align: middle; min-height: 99px;">
        <a href="/">
          <img src="/img/scrive-logo-dark.png" style="max-height: 46px; max-width: 350px; margin: 0; padding-top: 26px; padding-bottom: 26px; border: 0;" />
        </a>
      </div>
    </div>

    <div style="margin: 0 auto; width: 550px; margin-top: 112px;">
      <div style="text-align: center; margin-bottom: 26px; font-size: 72pt; letter-spacing: -20; color: #D9D9D9;">
        $_unsupportedBrowserSorry()$
      </div>
      <div style="text-align: center; font-size: 14.5pt; letter-spacing: -20; color: #4D4D4D;">
        $_unsupportedBrowserHeader()$
      </div>
      <div id="browser_name" style="text-align: center; font-size: 18pt; letter-spacing: -20; color: #4D4D4D; margin-bottom: 14px;">
      </div>
      <div style="margin-bottom: 50px;">
        <center>
          <img src="/img/IE-logo-circle.png"/>
        </center>
      </div>
      <div style="margin-bottom: 18px; font-size: 14.5pt; letter-spacing: -20; color: #4D4D4D;">
        $_unsupportedBrowserInstructionsHeader()$
      </div>
      <div style="font-size: 14.5pt; letter-spacing: -20; color: #4D4D4D;">
        <div>1. $_unsupportedBrowserInstruction1()$</div>
        <div>2. $_unsupportedBrowserInstruction2()$</div>
      </div>
    </div>

  </body>
$htmlclosingtag()$
