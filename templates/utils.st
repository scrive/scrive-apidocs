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
PoweredByScrive=E-signing powered by Scrive
###
### All the prices below are when paying yearly.
###
OnePriceSEKPerMonth=300
###
OnePriceEURPerMonth=30
###
OnePriceWithDiscountSEKPerMonth=240
###
OnePriceWithDiscountEURPerMonth=24
###
TeamPriceSEKPerMonth=4500
###
TeamPriceEURPerMonth=450
###
TeamPriceWithDiscountSEKPerMonth=3600
###
TeamPriceWithDiscountEURPerMonth=360
###
EnterprisePriceSEKPerMonth=45000
###
EnterprisePriceEURPerMonth=4500
###
EnterprisePriceWithDiscountSEKPerMonth=36000
###
EnterprisePriceWithDiscountEURPerMonth=3600
###
CompanyPriceSEKPerMonth=15000
###
CompanyPriceEURPerMonth=1500
###
CompanyPriceWithDiscountSEKPerMonth=12000
###
CompanyPriceWithDiscountEURPerMonth=1200
###
CompanyDocumentsPerMonth=2 500
###
EnterpriseDocumentsPerMonth=10 000
###
TeamDocumentsPerMonth=500
###

OneDocumentsPerMonth=25
####
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
PaymentYearlyDiscountAmount=20%
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