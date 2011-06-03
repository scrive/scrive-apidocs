###
priceplanPage=
    $doctype()$
    <html>
      <head>
        <title>$pagetitle()$ | Prisplan</title>
        <meta name="description" content="Signera och arkivera dina avtal elektroniskt. Välj prisplan efter dina behov – Free, Small, Business eller Enterprise." />
        $standardHeaderContents()$
        $if(production)$
        <script src="$prefix$/js/analytics.js" type="text/javascript"></script>
        $endif$
      </head>
      <body>
         $bodystart()$
         <div class="mainContainer">
           $priceplanPageContents()$
         </div>
         $bodyend()$ 
      </body>
    </html>
###
priceplanPageContents=
                <div class="big-price-plan">
                    <h1>Priser</h1>
                    <p class="stronger">Skriva på dokument snabbt, smidigt och säkert!</p>
                    <p class="stronger">Registrera dig på under minuten. Säg upp när som helst.</p>
                </div>
                <div class="big-pp-container">
                    <table>
                        <thead>
                            <tr>
                                <th>
                                    <h2>Prisplan</h2>
                                </th>
                                <th>
                                    <h3>Small</h3>
                                    <p>249 kr/mån</p>
                                </th>
                                <th>
                                    <h3>Medium</h3>
                                    <p>499 kr/mån</p>
                                </th>
                                <th>
                                    <h3>Business</h3>
                                    <p>999 kr/mån</p>
                                </th>
                                <th>
                                    <h3>Enterprise</h3>
                                    <p>Offert</p>
                                </th>
                            </tr>
                        </thead>
                        <tbody>
                            <tr class="spacer">
                              <td></td>
                              <td></td>
                              <td></td>
                              <td></td>
                              <td></td>
                            </tr>
                            <tr>
                                <td>Antal användare</td>
                                <td>1 användare</td>
                                <td>5 användare</td>
                                <td>15 användare</td>
                                <td>Obegränsat</td>
                            </tr>
                            
                            <tr class="odd-row">
                                <td>Fria signaturer per månad</td>
                                <td>10 fria signaturer/mån</td>
                                <td>20 fria signaturer/mån</td>
                                <td>50 fria signaturer/mån</td>
                                <td>Min 2.000 signaturer/år</td>
                            </tr>
                            <tr>
                                <td>Standardlagring</td>
                                <td>50 dok</td>
                                <td>500 dok</td>
                                <td>Obegränsad</td>
                                <td>Obegränsad</td>
                            </tr>
                            <tr class="odd-row">
                                <td>Support</td>
                                <td>E-post</td>
                                <td>Telefon</td>
                                <td>Telefon</td>
                                <td>Personlig kontakt</td>
                            </tr>
                            <tr >
                                <td>Extra signaturer per styck</td>
                                <td>Uppgradera</td>
                                <td>35 kr</td>
                                <td>25 kr</td>
                                <td>Offert</td>
                            </tr>
                            <tr  class="odd-row">
                                <td>Trustweaver premiumlagring</td>
                                <td>Uppgradera</td>
                                <td>Offert</td>
                                <td>Offert</td>
                                <td>Offert</td>
                            </tr>
                            <tr>
                                <td>SkrivaPå Tredjepartsverifiering</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr class="odd-row">
                                <td>SkrivaPå bevisgenerering</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr>
                                <td>Global access 24/7</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr class="odd-row">
                                <td>Fria uppdateringar</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr>
                                <td>E-signering</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr class="odd-row">
                                <td>E-legitimationssignering</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr>
                                <td>Offertbekräftelser</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr class="odd-row">
                                <td>Skydd mot adressfel</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr>
                                <td>E-arkiv</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr class="odd-row">
                                <td>Autoarkivering</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr>
                                <td>Sökfunktionalitet</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr class="odd-row">
                                <td>Processbevakning</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr>
                                <td>Påminnelser</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr class="odd-row">
                                <td>Hälsningsmeddelande</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr>
                                <td >Välj förfallodatum</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr class="odd-row">
                                <td >Justera dokumentnamn</td>
                                <td></td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr>
                                <td >Mallar</td>
                                <td></td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr class="odd-row">
                                <td >Bilagor</td>
                                <td></td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr>
                                <td >Flerpartsavtal</td>
                                <td></td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr class="odd-row">
                                <td >Sekreterarfunktionalitet</td>
                                <td></td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr>
                                <td >Drag and drop</td>
                                <td></td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr class="odd-row">
                                <td >Signeringsordning</td>
                                <td></td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr>
                                <td >Dela dokumentarkiv</td>
                                <td></td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr class="odd-row">
                                <td >Enkel Integration</td>
                                <td></td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr>
                                <td >Brandad signeringssida</td>
                                <td></td>
                                <td></td>
                                <td></td>
                                <td>✓</td>
                            </tr>
                            <tr class="odd-row">
                                <td >Massutskick</td>
                                <td></td>
                                <td></td>
                                <td></td>
                                <td>✓</td>
                            </tr>
                            <tr>
                                <td >Massifyllning</td>
                                <td></td>
                                <td></td>
                                <td></td>
                                <td>✓</td>
                            </tr>
                            <tr class="odd-row">
                                <td >Masspåminnelser</td>
                                <td></td>
                                <td></td>
                                <td></td>
                                <td>✓</td>
                            </tr>
                            <tr>
                                <td >Fullt API</td>
                                <td></td>
                                <td></td>
                                <td></td>
                                <td>✓</td>
                            </tr>
                        </tbody>
                    </table>
                    <div class="highlight-container ppfix">
                        <div class="ribbon"></div>
                        <ul class="highlight-rows">
                            <li class="top">
                                <h3>Business</h3>
                                <p>999kr/mån</p>
                            </li>
                            <li>15 användare</li>
                            <li class="odd">50  fria signaturer/mån</li>
                            <li>Obegränsad</li>
                            <li class="odd">Telefon</li>
                            <li>25 kr</li>
                            <li class="odd">Offert</li>
                            <li>✓</li>
                            <li class="odd">✓</li>
                            <li>✓</li>
                            <li class="odd">✓</li>
                            <li>✓</li>
                            <li class="odd">✓</li>
                            <li>✓</li>
                            <li class="odd">✓</li>
                            <li>✓</li>
                            <li class="odd">✓</li>
                            <li>✓</li>
                            <li class="odd">✓</li>
                            <li>✓</li>
                            <li class="odd">✓</li>
                            <li>✓</li>
                            <li class="odd">✓</li>
                            <li>✓</li>
                            <li class="odd">✓</li>
                            <li>✓</li>
                            <li class="odd">✓</li>
                            <li>✓</li>
                            <li class="odd">✓</li>
                            <li>✓</li>
                            <li class="odd">✓</li>
                            <li></li>
                            <li class="odd"></li>
                            <li></li>
                            <li class="odd"></li>
                            <li class="bottom"></li>
                        </ul>
                    </div>
                </div>
                <div class="drop-shadow-container">
                    <div class="drop-shadow"></div>
                </div>
                <br />
                <br />
                <div class="big-price-plan">
                    <h2>Kan jag byta prisplan när som helst?</h2>
                    <p>SkrivaPå är baserad på månadsprenumeration. Ingen bindningstid, inga konstigheter. Du kan uppgradera, nedgradera eller avsluta ditt konto när du vill. Kontakta oss gärna om du behöver hjälp med att välja det paket som passar dina behov. Du kan nå oss på e-post <a href="mailto:info@skrivapa.se">info@skrivapa.se</a> eller telefon 010-195 99 15. </p>
                </div>
                <div class="big-price-plan">
                                    <h2>15% rabatt och större flexibilitet!</h2>
                                    <p> För Small och Business kan du istället betala för 1 år och få 15% rabatt på priset. 
                                        Dessutom kan du då fördela de fria signaturerna över hela året. </p>
                </div>
###
