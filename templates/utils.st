#########             UTILS           #######
####
strong=
    <strong>$it$</strong>
###
morethenonelist=
    $list:strong(); separator=", "$ och $last:strong()$
###
###
nomorethanonelist=
    $list:strong()$
###
customFooter=
    <p>
      Hälsningar
      <br/>
      $creatorname$
    </p>
###
loginPageRedirectReason=
$if(notlogged)$
  Du är inte inloggad.
$endif$
$if(notsu)$
  Du har inte rätt behörighet att se denna sida.
$endif$
$if(invemail)$
  Felaktig e-post.
$endif$
$if(invpass)$
  Felaktigt lösenord.
$endif$
###
poweredBySkrivaPaPara=
    <p>
      <small>Med vänliga hälsningar<br/><a href="$ctxhostpart$">SkrivaPå</a></small>
    </p>
#####
wrapHTML=
    <html>
      <head>
        <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
      </head>
      <body>
        $body:noescape()$
      </body>
    </html>
####
makeEditable=
    <div class="editable" name="$name$">$this:noescape()$</div>
####
replaceOnEdit=
    <span>
      <span class="replacebynextonedit"> $this:noescape()$ </span>
      <span style="display:none"> $with:noescape()$ </span>
    </span>
#####
actionButton=
    <form method="POST" action="$action$">
      <input type="submit" value="$buttonname$" />
    </form>
#####
LinkLogin=/login
###
LinkForgotPassword=/amnesia
###
LinkLandpageSaved=/landpage/signedsave/$documentid$/$signatorylinkid$
###
###
join=$fst$$snd$
###
option=<option value="$oValue$" $if(oSelected)$ SELECTED $endif$ > $oText$ </option>
###
options=$it:{n | $option(oValue=n.oValue;oText=n.oText;oSelected=n.oSelected)$}$
###
input=<input style="width:50px" type="$iType$" name="$iName$" value="$iValue$">
###
showmoney=$main$.$rest$
###
br=$it:noescape()$<br/>
###
span=<span>$it:noescape()$</span>
###
td=<td style="text-align:center">$it:noescape()$</td>
###
###
th=<th style="width:60px;text-align:center">$it:noescape()$</th>
###
spanNoEscape=<span>$it:noescape()$</span>
####
facebooklogo=
<link rel="image_src" href="https://skrivapa.se/img/facebooklogo.jpg" />
<meta name="title" content="SkrivaPå" />
<meta name="description" content="Dokumenthantering är tidskrävande. Tid är pengar. SkrivaPå är ett skräddarsytt redskap som optimerar dina processer. Spara 50-250kr/dokument." />
####
#### Puts an invisible box with flash messages. jScripts do all show magic.
####
flashmessages = 
<div class="flashmsgbox">
            $flashmessages:{ msg | <div class="flashmessage">$msg:noescape()$</div> }$
</div>
####
