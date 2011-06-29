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
                                    <p>4 995 kr/år</p>
                                </th>
                                <th>
                                    <h3>Medium</h3>
                                    <p>14 995 kr/år</p>
                                </th>
                                <th>
                                    <h3>Business</h3>
                                    <p>29 995 kr/år</p>
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
                                <td>Uppstartskostnad</td>
                                <td>5 000 kr</td>
                                <td>5 000 kr</td>
                                <td>10 000 kr</td>
                                <td>Offert</td>
                            </tr>
                            <tr class="odd-row">
                                <td>Uppstartsutbildning</td>
                                <td>30 min</td>
                                <td>60 min</td>
                                <td>2 x 60 min</td>
                                <td>Offert</td>
                            </tr>
                            <tr>
                                <td>Hjälp med konfiguration</td>
                                <td>1 h</td>
                                <td>2 h</td>
                                <td>4 h</td>
                                <td>Offert</td>
                            </tr>
                            <tr class="odd-row">
                                <td>Antal användare</td>
                                <td>5 användare</td>
                                <td>15 användare</td>
                                <td>50 användare</td>
                                <td>Offert</td>
                            </tr>
                            <tr>
                                <td>Fria signaturer</td>
                                <td>250 sign/år</td>
                                <td>750 sign/år</td>
                                <td>1 500 sign/år</td>
                                <td>Från 3 000 sign/år</td>
                            </tr>
                            <tr class="odd-row">
                                <td>Extra signaturer per styck</td>
                                <td>40 kr</td>
                                <td>35 kr</td>
                                <td>35 kr</td>
                                <td>Offert</td>
                            </tr>
                            <tr>
                                <td>Standardlagring</td>
                                <td>500 dok</td>
                                <td>Obegränsad</td>
                                <td>Obegränsad</td>
                                <td>Offert</td>
                            </tr>
                            <tr class="odd-row">
                                <td>Trustweaver premiumlagring</td>
                                <td>Uppgradera</td>
                                <td>Offert</td>
                                <td>Offert</td>
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
                                <p>29 995 kr/år</p>
                            </li>
                            <li>10 000 kr</li>
                            <li class="odd">2 x 60 min</li>
                            <li>4 h</li>
                            <li class="odd">50 användare</li>
                            <li>1 500  sign/år</li>
                            <li class="odd">35 kr</li>
                            <li>Obegränsad</li>
                            <li class="odd">Offert</li>
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
                <div class="big-price-plan">
                    <h3>Fri användning</h3>
                    <p>Vi erbjuder också ett frikonto för 1 användare, 5 fira signaturer per månad och 50 dokument i lagring.</p>
                </div>
###
