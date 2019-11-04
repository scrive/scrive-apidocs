#########             UTILS           #######
####
strong=
    <strong>$it$</strong>
###
br=$it$<BR/>
###
morethenonelist=
    $list:strong(); separator=", "$ $_listand()$ $last:strong()$
###
nomorethanonelist=$list:strong()$
###
#### Puts an invisible box with flash messages. jScripts do all show magic.
#### Bellow is a hack for (if(flash)) for String templates syntax
####
flashmessages =
<script src="$cdnbaseurl$/compiled/all/flashmessage-$versioncode$.js" crossorigin="anonymous"></script>
####
currentSubscription=
<script>
        var subscriptionData;
        try {
          subscriptionData = JSON.parse(Base64.decode("$b64subscriptiondata$"));
        } catch (e) {
          console.log("Could not parse subscription data embedded in the DOM", e);
        }

        var fromCurrentSubscriptionTemplate = {
          subscriptionData: subscriptionData,
          subscriptionUserIsCompanyAdmin: $subscriptionuseriscompanyadmin$
        };
</script>
<script src="$cdnbaseurl$/compiled/all/current_subscription-$versioncode$.js" crossorigin="anonymous"></script>
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
IcelandicInIcelandic=Íslenska
###
EstonianInEstonian=Eesti
###
LatvianInLatvian=Latviešu
###
LithuanianInLithuanian=Lietuvių
###
CzechInCzech=Čeština
###
PolishInPolish=Polski
###
HungarianInHungarian=Magyar
###
PoweredByScrive=E-signing powered by Scrive
###
### All the prices below are when paying yearly.
###
OnePriceSEKPerMonth=300
###
OnePriceEURPerMonth=30
###
TeamPriceSEKPerMonth=4500
###
TeamPriceEURPerMonth=450
###
TeamPriceMonthlySEKPerMonth=5620
###
TeamPriceMonthlyEURPerMonth=562
###
CompanyPriceSEKPerMonth=15000
###
CompanyPriceEURPerMonth=1500
###
CompanyPriceMonthlySEKPerMonth=18750
###
CompanyPriceMonthlyEURPerMonth=1875
###
OnePlanName=One
####
TeamPlanName=Team
####
CompanyPlanName=Company
####
EnterprisePlanName=Enterprise
###
PaymentCurrencyEUR=€
###
PaymentCurrencySEK=kr
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
basicAuth=Basic Auth
###
basicAuthDescription=Basic Auth adds an Authorization header to all callbacks.
###
attachmentNameOrPlaceholder=$if(attachment_name)$$attachment_name$$else$<span class='put-attachment-name-here'></span>$endif$
###
verimiText=Verimi
###
identifyWithVerimi=Identify with Verimi
###
idinText=Dutch iDIN
###
identifyWithIdin=Identify with iDIN
###
