#########             UTILS           #######
####
strong=
    <strong>$it$</strong>
###
morethenonelist=
    $list:strong(); separator=", "$ $_listand()$ $last:strong()$
###
###
nomorethanonelist=
    $list:strong()$
###
listyou=
    $_listyou()$
###
customFooter= <span/>
###
poweredByScrive=
    <p>
      <small> $_poweredByScrive()$ <br/><a href="$ctxhostpart$">Scrive</a></small>
    </p>
#####
wrapHTML=
    <html>
      <head>
        <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
        <meta http-equiv="content-language" content="$ctxlang$"/>
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
LinkLogin=/login
###
LinkForgotPassword=/amnesia
###
LinkLandpageSaved=/landpage/signedsave/$documentid$/$signatorylinkid$
###
###
join=$fst$$snd$
###
option=<option value="$oValue$" $if(oSelected)$ SELECTED="SELECTED" $endif$ > $oText$ </option>
###
options=$it:{n | $option(oValue=n.oValue;oText=n.oText;oSelected=n.oSelected)$}$
###
input=<input style="width:50px" type="$iType$" name="$iName$" value="$iValue$"/>
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
#### Puts an invisible box with flash messages. jScripts do all show magic.
####
flashmessages =
<div class="flashmsgbox"> </div>
<script>
   jQuery(function(){
     $flashmessages:{ flash |
         $if(!flash.isModal)$
            FlashMessages.add({
              "color" : "$flash.type$",
              "content": "$flash.message:noescape()$"
            })
         $endif$
     }$
   })
</script>
<div class="modalcontainer">
$flashmessages:{ flash |
    $if(flash.isModal)$
        <div class="modalbox overlay">
            $flash.message:noescape()$
        </div>
    $endif$
}$
</div>
####
####
loadingdialog=
<div id="loadingdialog" class="overlay">
  <a href="#" id="loadinglink" style="display:none;" rel="#loadingdialog"></a>
  <div class="modal-container">
    <div class="modal-body" style="padding:30px">
      <center>
        <h3 id="loadingmessage">Loading</h3>
        <img src="/img/wait30trans.gif" style="margin:30px"/>
      </center>
    </div>
  </div>
</div>
####
englishHasSP=$if(pl)$have$else$has$endif$
##
