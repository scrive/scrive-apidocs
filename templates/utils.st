#########             UTILS           #######
####
strong=
    <strong>$it$</strong>
###
morethenonelist=
    $list:strong(); separator=", "$ $_listand()$ $last:strong()$
###
nomorethanonelist=$list:strong()$
###
morethenonelistnormal=$list; separator=", "$ $_listand()$ $last$
###
nomorethanonelistnormal=
    $list$
###
makeEditable=
    <div class="editable" name="$name$">$this:noescape()$</div>
####
#### Puts an invisible box with flash messages. jScripts do all show magic.
#### Bellow is a hack for (if(flash)) for String templates syntax
####
flashmessages =
<script>
 require(['jquery', 'legacy_code'], function(jQuery) {
   jQuery(function(){
     $flashmessages:{ flash |
        $if(flash)$
            new FlashMessage({
              "type" : "$flash.type$",
              "content": "$flash.message:noescape()$"
            })
        $endif$
     }$
   })
 });
</script>

####
EnglishInEnglish=English
####
SwedishInSwedish=Svenska
####
GermanInGerman=Deutsch
####
FrenchInFrench=Français
####
ItalianInItalian=Italiano
####
SpanishInSpanish=Español
####
PortugueseInPortuguese=Português
####
DutchInDutch=Nederlands
####
DanishInDanish=Dansk
####
NorwegianInNorwegian=Norsk
####
GreekInGreek=ελληνικά
###
FinnishInFinish=Suomi
###
PoweredByScrive=E-signing powered by Scrive
###
TeamPricePerMonth=200
###
FormPricePerMonth=3000
###
### This represents the number of users
###
FormPrice50=50
###
### Texts for integration settings - they are not translated, since this only affect very specyfic users
###
salesforce=Salesforce
###
salesforceDescription=Salesforce integration is active for this account
###
constantUrlCallback=Callbacks
###
constantUrlCallbackDescription=All callbacks for this account will be redirected to <strong class='put-url-here'/>
###
