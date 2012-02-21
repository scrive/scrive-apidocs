###
priceplanPage=
    $doctype()$
    <html>
      <head>
        <title>$pagetitle()$ $_priceplanPageTitle()$</title>
        <meta name="description" content="$_priceplanPageMetaDescription()$" />
        $standardHeaderContents()$
        $if(production)$
        <script src="$prefix$/js/analytics.js" type="text/javascript"></script>
        $endif$
      </head>
      $standardBody(unwrappedbody=priceplanPageContents())$
    </html>
###
priceplanPageContents=
                <div class="big-price-plan">
                    <h1>$_priceplanPageH1()$</h1>
                    <p class="stronger">$_priceplanPageH1Body1()$</p>
                    <p class="stronger">$_priceplanPageH1Body2()$</p>
                </div>
                <div class="big-pp-container">
                    <table>
                        <thead>
                            <tr>
                                $_priceplanPagePriceplanTop()$
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
                            <tr>$_priceplanPageTableRow1()$
                            </tr>
                            <tr class="odd-row">$_priceplanPageTableRow2()$
                            </tr>
                            <tr>$_priceplanPageTableRow3()$
                            </tr>
                            <tr class="odd-row">$_priceplanPageTableRow4()$
                            </tr>
                            <tr>$_priceplanPageTableRow5()$
                            </tr>
                            <tr class="odd-row">$_priceplanPageTableRow6()$
                            </tr>
                            <tr>$_priceplanPageTableRow7()$
                            </tr>
                            <tr class="odd-row">$_priceplanPageTableRow8()$
                            </tr>
                            <tr>$_priceplanPageTableRow9()$
                            </tr>
                            <tr class="odd-row">$_priceplanPageTableRow10()$
                            </tr>
                            <tr>$_priceplanPageTableRow11()$
                            </tr>
                            <tr class="odd-row">$_priceplanPageTableRow12()$
                            </tr>
                            <tr>$_priceplanPageTableRow13()$
                            </tr>
                            <tr class="odd-row">$_priceplanPageTableRow14()$
                            </tr>
                            <tr>$_priceplanPageTableRow15()$
                            </tr>
                            <tr class="odd-row">$_priceplanPageTableRow16()$
                            </tr>
                            <tr>$_priceplanPageTableRow17()$
                            </tr>
                            <tr class="odd-row">$_priceplanPageTableRow18()$
                            </tr>
                            <tr>$_priceplanPageTableRow19()$
                            </tr>
                            <tr class="odd-row">$_priceplanPageTableRow20()$
                            </tr>
                            <tr>$_priceplanPageTableRow21()$
                            </tr>
                            <tr class="odd-row">$_priceplanPageTableRow22()$
                            </tr>
                            <tr>$_priceplanPageTableRow23()$
                            </tr>
                            <tr class="odd-row">$_priceplanPageTableRow24()$
                            </tr>
                            <tr>$_priceplanPageTableRow25()$
                            </tr>
                            <tr class="odd-row">$_priceplanPageTableRow26()$                            </tr>
                            <tr>$_priceplanPageTableRow27()$
                            </tr>
                            <tr class="odd-row">$_priceplanPageTableRow28()$                            </tr>
                            <tr>$_priceplanPageTableRow29()$
                            </tr>
                            <tr class="odd-row">$_priceplanPageTableRow30()$                            </tr>
                        </tbody>
                    </table>
                    <div class="highlight-container ppfix">
                        <div class="ribbon  $if(langen)$english$else$swedish$endif$"></div>
                        <ul class="highlight-rows">$_priceplanPageTableHighlightedColumn()$
                        </ul>
                    </div>
                </div>
                <br />
                <br />
                <div>$_priceplanPageVAT()$</div>
                <div class="big-price-plan">$_priceplanPageBelowTable1()$
                </div>
                <div class="big-price-plan">$_priceplanPageBelowTable2()$
                </div>
                <div class="big-price-plan">$_priceplanPageBelowTable3()$
                </div>
###
