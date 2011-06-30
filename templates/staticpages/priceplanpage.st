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
                    <p class="stronger">Registrera dig på under minuten och testa 14 dagar redan idag.</p>
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
                                    <p>35 kr/signatur</p>
                                </th>
                                <th>
                                    <h3>Team</h3>
                                    <p>32.5 kr/signatur</p>
                                </th>
                                <th>
                                    <h3>Business</h3>
                                    <p>30 kr/signatur</p>
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
                                <td>5 användare</td>
                                <td>15 användare</td>
                                <td>50 användare</td>
                                <td>Offert</td>
                            </tr>
                            <tr class="odd-row">
                                <td>Minimum antal signaturer per år</td>
                                <td>500</td>
                                <td>1 000</td>
                                <td>2 000</td>
                                <td>4 000</td>
                            </tr>
                            <tr>
                                <td>Standardlagring</td>
                                <td>500 dok</td>
                                <td>Obegränsad</td>
                                <td>Obegränsad</td>
                                <td>Obegränsad</td>
                            </tr>
                            <tr class="odd-row">
                                <td>Trustweaver premiumlagring</td>
                                <td>Uppgradera</td>
                                <td>Uppgradera</td>
                                <td>Offert</td>
                                <td>Offert</td>
                            </tr>
                            <tr>
                                <td>Uppstartsutbildning</td>
                                <td>0.5 h</td>
                                <td>1 h</td>
                                <td>2 x 1 h</td>
                                <td>Offert</td>
                            </tr>
                            <tr class="odd-row">
                                <td>Hjälp med konfiguration</td>
                                <td>1 h</td>
                                <td>2 h</td>
                                <td>4 h</td>
                                <td>Offert</td>
                            </tr>
                            <tr>
                                <td>Support</td>
                                <td>E-post</td>
                                <td>Telefon</td>
                                <td>Telefon</td>
                                <td>Personlig kontakt</td>
                            </tr>
                            <tr class="odd-row">
                                <td>Avtals-, offert- eller orderprocess</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr>
                                <td>Tredjepartsverifiering</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr class="odd-row">
                                <td>E-signering</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr>
                                <td>E-legitimationssignering</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr class="odd-row">
                                <td>E-arkiv</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr>
                                <td>Processbevakning</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr class="odd-row">
                                <td>Påminnelser</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr>
                                <td>Justera hälsningsmeddelande</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr class="odd-row">
                                <td>Välj förfallodatum</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr>
                                <td>Spara egna mallar</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr class="odd-row">
                                <td>Bifoga bilaga</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr>
                                <td>Begär Bilagor</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr class="odd-row">
                                <td>Flerpartsavtal</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr>
                                <td>Användarroller</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr class="odd-row">
                                <td >Drag and drop</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr>
                                <td>Signeringsordning</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr class="odd-row">
                                <td>Delat arkiv</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr>
                                <td>Enkel Integration</td>
                                <td></td>
                                <td></td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr class="odd-row">
                                <td>Massutskick</td>
                                <td></td>
                                <td></td>
                                <td>✓</td>
                                <td>✓</td>
                            </tr>
                            <tr>
                                <td>Brandad signeringssida</td>
                                <td></td>
                                <td></td>
                                <td></td>
                                <td>✓</td>
                            </tr>
                            <tr class="odd-row">
                                <td>Brandat mail</td>
                                <td></td>
                                <td></td>
                                <td></td>
                                <td>✓</td>
                            </tr>
                            <tr>
                                <td>Fullt API</td>
                                <td></td>
                                <td></td>
                                <td></td>
                                <td>✓</td>
                            </tr>
                            <tr class="odd-row">
                                <td>Skapa egen process</td>
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
                                <p>30 kr/signatur</p>
                            </li>
                            <li>50</li>
                            <li class="odd">2 000</li>
                            <li>Obegränsad</li>
                            <li class="odd">Offert</li>
                            <li>2 x 1h</li>
                            <li class="odd">4h</li>
                            <li>Telefon</li>
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
                <div>* Alla priser är exkl moms.</div>
                <div class="big-price-plan">
                    <h2>Vi tar bara betalt när du tjänar pengar</h2>
                    <p>Vi tar betalt per signatur. Vi tror dock på att bara ta betalt när vi tillför som mest värde. Därför räknar vi bara signaturer i färdigställda dokument där vi utfärdat vårt verifikat. Med andra ord, vi tar bara betalt när du gör affär.Vi tar betalt per signatur. Vi tror dock på att bara ta betalt när vi tillför som mest värde. Därför räknar vi bara signaturer i färdigställda dokument där vi utfärdat vårt verifikat. Med andra ord, vi tar bara betalt när du gör affär.</p>
                </div>
                <div class="big-price-plan">
                    <h2>Gratis för privatpersoner och egenföretagare</h2>
                </div>
###
