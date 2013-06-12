#########             UTILS           #######
####
strong=
    <strong>$it$</strong>
###
morethenonelist=
    $list:strong(); separator=", "$ $_listand()$ $last:strong()$
###
nomorethanonelist=
    $list:strong()$
###
morethenonelistnormal=
    $list; separator=", "$ $_listand()$ $last$
###
nomorethanonelistnormal=
    $list$
###
makeEditable=
    <div class="editable" name="$name$">$this:noescape()$</div>
####
input=<input style="width:50px" type="$iType$" name="$iName$" value="$iValue$"/>
###
br=$it:noescape()$<br/>
###
td=<td style="text-align:center">$it:noescape()$</td>
###
###
th=<th style="width:60px;text-align:center">$it:noescape()$</th>
###
#### Puts an invisible box with flash messages. jScripts do all show magic.
####
flashmessages =
<script>
   jQuery(function(){
     $flashmessages:{ flash |
         $if(!flash.isModal)$
            new FlashMessage({
              "color" : "$flash.type$",
              "content": "$flash.message:noescape()$"
            })
         $endif$
     }$
   })
</script>

$flashmessages:{ flash |
    $if(flash.isModal)$
          $flash.message:noescape()$
    $endif$
}$
####
