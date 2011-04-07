####
showFileImagesReady= $images:{image| 

<div id="page$image.number$" class="pagediv">
  <div class="pagejpg" style="background-image: url(/pages/$doctokenid$/$doctokenhash$/$fileid$/$image.number$);">
    <input type="hidden" name="width" value="$image.width$"> 
    <input type="hidden" name="height" value="$image.height$">
  </div>
</div> 
}$
####
showFileImagesPending= 
<div class="pagejpga4 pagejpg">
  <img class="waiting" src="/theme/images/wait30trans.gif">
</div>
####
showFileImagesError=
<div class="errormsg">
 Vi beklagar, det var något problem med formateringen på din PDF. Vänligen kontrollera dokumentet och försök igen.
</div>
####
###
####
pageDocumentForViewerHelpers=
<span class="templating_insert"> 
  <div id="signatory_template" class="signatorybox" alt="Namn på avtalspart">
  <input name="signatoryname" type="text" value="" autocomplete="off"
         infotext="Fullständigt namn" /><br/>
  <input name="signatorycompany" type="text" value="" autocomplete="off"
         infotext="Företag" /><br/>
  <input name="signatorypersonalnumber" type="text" value="" autocomplete="off"
         infotext="Pers.nr" /><br/>
  <input name="signatorycompanynumber" type="text" value="" autocomplete="off"
         infotext="Org.nr" /><br/>
  <input name="signatoryemail"  type="email" value="" autocomplete="off"
         infotext="E-post" /><br/>
  <small><a onclick="return signatoryremove(this.parentNode);" href="#">Ta bort</a></small>
  </div>
</span>,
<script type="text/javascript"> 
  var documentid = "$documentid$", doctokenid = "$doctokenid$", doctokenhash = "$doctokenhash$";
</script>
###
pageDocumentForViewerContent = 
  <div class="localdialogs">
    <div style="display: none">
      $helpers:noescape()$
    </div>
  </div>
<div id="signStepsContainer"> 
  <div id="signStepsTitleRow"> 
     <span id="signStepsTitleRowTextContainer"> 
      <span class="title">DOKUMENT #$documentid$:</span>
      <span class="name">$documenttitle$</span>
     </span>
     <span class="download">
        <a href="$linkissuedocpdf$" target="_blank">
         Ladda ned PDF
       </a>
     </span>
  </div>
  <div id="signStepsBody" class="forauthor"> 
    <div id="signViewBodyLeft" > 
      <span id="signViewBodyLeftTextContainer"> 
          $documentinfotext:noescape()$
      </span> 

  <div> 
    
  </div>

    </div>
      $signatoriesBox()$
  </div>
</div>

<div id="documentBox">
  <script type="text/javascript">
    var docstate = $docstate:noescape()$;
  </script>

  <div class="waiting4page"></div>
</div>

$signatoryScriptIfRequired()$

#####
pageDocumentForSignHelpers=
<script type="text/javascript"> var documentid = "$documentid$", doctokenid = "$doctokenid$", doctokenhash = "$doctokenhash$";
</script>
<script type="text/javascript">
  $localscripts:noescape()$
</script>
######
pageDocumentForSignContent=
  <script type="text/javascript" src="/js/bankid.js"></script>
  <div class="localdialogs">
    $loadingdialog()$
    <div style="display: none">
      $helpers:noescape()$
    </div>
    <form method="post" name="form" action="$action$" id="dialog-confirm-sign" class="overlay">     
    <div class="modal-container">
        <div class="modal-header">
            <div class="modal-icon message"></div>
            <div class="modal-title">
               <h2>Underteckna</h2> 
            </div>
            <a class="modal-close close"> </a>
        </div>
        <div class="modal-body">
            <div class="modal-content">
                 <p>Är du säker att du vill underteckna dokumentet <strong>$documenttitle$</strong>?</p>
                 <p>När $partyUnsigned:noescape()$ undertecknat blir 
                    avtalet <strong>juridiskt bindande</strong> och
                    det färdigställda avtalet skickas till din
                    e-post.</p>
                 $if(requireseleg)$
                 <span>
                   <h3>Välj typ av e-legitimation</h3>
                   <p>Har du ingen e-legitimation? Du kan ladda ned en
                     e-legitimation från din internetbank eller från
                     Telia. <a href="http://www.e-legitimation.se/Elegitimation/Templates/LogolistPageTypeB.aspx?id=86"
                               target="_new">Klicka här</a> för att läsa mer.</p>
                 </span>
                 $endif$
            </div>
        </div>
        <div class="modal-footer">
          <input type="hidden" name="sign" value="automatic" id="signauto" />
          <a  class="close cancel">Avbryt</a>
          $if(requireseleg)$
          <span id="elegbuttons">
            <input type="hidden" name="changetoeleg" id="elegprovider" value=""
                   /> 
            <input type="hidden" name="signature" value="" id="signatureinput" />
            <input type="hidden" name="transactionid" value="" id="transactionidinput" />

            <a href="#" class="btn-small green float-right signatory bankid">
              <img src="/img/bankid.png" alt="BankID" />
            </a>

            <a href="#" class="btn-small green float-right signatory telia">
              <img src="/img/telia.png" alt="Telia Eleg" />
            </a>

            <a href="#" class="btn-small green float-right signatory nordea">
              <img src="/img/nordea.png" alt="Nordea Eleg" />
            </a>
          </span>
          $else$
          
          <a href="#" class="btn-big blue float-right submiter">
            <div class="left"></div>
            <div class="label">
              <span>SkrivaPå</span>
              <span class="icon cross"></span>
            </div>
            <div class="right"></div>
          </a>
          $endif$
        </div>
     </div>

    </form>

    <form method="post" name="form" action="$action$" id="dialog-confirm-cancel" class="overlay">   
    <div class="modal-container">
        <div class="modal-header">
            <div class="modal-icon message"></div>
            <div class="modal-title">
               <h2>Avvisa</h2>   
                <p>Är du säker på att du vill avvisa dokumentet <strong>$documenttitle$</strong>?</p>
                <p>När du avvisat kommer vi att skicka ett e-postmeddelande för att meddela <strong>$authorfstname$ $authorsndname$</strong>.</p>
            </div>
            <a class="modal-close close"> </a>
        </div>
        <div class="modal-body">
            <div class="modal-content">
               $rejectMessage:noescape()$
            </div>
        </div>
        <div class="modal-footer">
                <input type="hidden" name="cancel" value="automatic" />
                <a  class="close cancel">Avbryt</a>
                <a  class="editer message">Skriv eget meddelande</a>
                <a href="#" class="btn-small red float-right submiter">
                    <div class="left"></div>
                    <div class="label">
                        <p>Avvisa </p>
                    <div class="btn-symbol red arrow-left"></div></div>
                    <div class="right"></div>
                </a>
        </div>
     </div>
    </form> 

  </div>
<div id="signStepsContainer"> 
  <div id="signStepsTitleRow">
    <span id="signStepsTitleRowTextContainer"> 
      <span class="title">DOKUMENT #$documentid$:</span>
      <span class="name">$documenttitle$</span>
    </span>
     <span class="download">
        <a href="$linkissuedocpdf$" target="_blank">
         Ladda ned PDF
       </a>
     </span>
  </div>
  <div id="signStepsBody"> 
    <div id="signViewBodyLeft" > 
      <span id="signViewBodyLeftTextContainer"> 
        $documentinfotext:noescape()$
      </span> 
    </div>
      $signatoriesBox()$
  </div>
</div>

<div id="documentBox">
  <div class="waiting4page"></div>
</div>
$if(notsignedbyme)$$if(pending)$
<div id="signViewBottomBox"> 
  <div id="signViewBottomBoxContainerLeft"> 
    <div id="signViewBottomBoxContainerLeftText"> 
     <div class="float-left"><input type="checkbox" id="signGuardCBox" class="signGuard" autocomplete="off" /></div>
     <div class="float-left" id="signGuardLabel"><label for="signGuardCBox">Jag intygar att jag granskat och förstått dokumentet, att samtliga parter är korrekta och att jag har befogenhet att skriva på.</label></div>
    </div>
  </div>
  <div id="signViewBottomBoxContainerRight"> 
  <a class="btn-big blue center submiter" id="sign" rel="#dialog-confirm-sign" name="sign">
    <div class="left"></div>
    <div class="label" style="width:120px">
      <span>SkrivaPå</span>
      <span class="icon cross"></span>
    </div>
    <div class="right"></div>
  </a>
  <a class="float-right cancel" name="cancel" rel="#dialog-confirm-cancel" id="cancel">Avvisa dokument</a>
  </div>
</div>
$endif$$endif$ 
$signatoryScriptIfRequired()$
###
####
pageDocumentForAuthorHelpersLocalDialogs=
<div class="localdialogs">
  $loadingdialog()$
  
  <form method="post" name="form" action="" class="overlay redirectsubmitform" id="dialog-csvupload-1" rel=".stepForm">
    <input type="hidden" name="csvupload" value="true" />
    <div class="modal-container">
      <div class="modal-header">
        <div class="modal-icon message"></div>
        <div class="modal-title">
          <h2>Massutskick</h2>
        </div>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <div>     
            Du har valt att göra ett massutskick, för att genomföra detta krävs att du laddar upp ett dokument med formatet .csv
          </div>
          <div class="signStepsBodyUploadBox">
            <div>
              <span id="signStepsBodyUploadTextContainer"> 
                <span class="header">Välj fil att ladda upp</span><br/>
                <span class="text">OBS! Endast .CSV</span> 
              </span>
            </div>
            <br/>
            <div class="signStepsButtonContainer">
              <a class="btn-small green">  
                <div class="left"> </div>
                <div class="label" style="width:80px"> Välj fil </div> 
                <div class="right"> </div>
                <input class="multiFileInput submitOnUpload" style="top:-40px;right:0px" maxlength="1" type="file" name="csv" accept="application/csv" rel=".filelist" />
              </a>
              <div class="filelist" style="display:none;"></div>
            </div>
          </div>
        </div>
      </div>
    </div>
  </form>
  
  <form method="post" name="form" action="" class="overlay redirectsubmitform" id="dialog-csvupload-2" rel=".stepForm">
    <input type="hidden" name="csvupload" value="true" />
    <div class="modal-container csv-dialog-2-container">
      <div class="modal-header">
        <div class="modal-icon message"></div>
        <div class="modal-title">
          <h2>Massutskick: Granska Data</h2>
        </div>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <div class="csvproblems">
            $csvproblems: {csvproblem |
              <div class="csvproblemcontainer$if(csvproblem.isfirstproblem)$ currentcsvproblem$endif$">
                <div class="csvproblemheader">
                  $if(csvproblem.problemrow)$Row: $csvproblem.problemrow$$endif$
                  $if(csvproblem.problemcol)$Column: $csvproblem.problemcol$$endif$
                </div>
                <div class="csvproblemdesc">
                  $csvproblem.problemdesc$
                </div>
                <div class="csvproblemfooter">
                  <div class="csvproblemcount">$csvproblem.problemnumber$/$csvproblemcount$</div>
                  $if(!csvproblem.isfirstproblem)$
                    <a class="btn-tiny black float-left previousproblem" href="#">
                      <div class="left"></div>
                      <div class="label">
                        <span class="btn-symbol black arrow-right"></span>
                        <span>Bakåt</span>
                      </div>
                      <div class="right"></div>
                    </a>
                  $endif$
                  $if(!csvproblem.islastproblem)$
                    <a class="btn-tiny black float-right nextproblem" href="#">
                      <div class="left"></div>
                      <div class="label">
                        <span>Nästa</span>
                        <span class="btn-symbol black arrow-left"></span>
                      </div>
                      <div class="right"></div>
                    </a>
                  $endif$
                </div>
              </div>
            }$
          </div>
          <div class="csvdata">
            $csvpages: {csvpage |
              <div class="csvpagecontainer$if(csvpage.isfirstcsvpage)$ currentcsvpage$endif$">
                <div class="csvpagecontents">
                  <table>
                    <theader>
                      <tr>
                        <th class="rownumbercol">&nbsp;</th>
                        <th>Förnamn</th>
                        <th>Efternamn</th>
                        <th>E-post</th>
                        <th>Företag</th>
                        <th>Pers. nr</th>
                        <th>Org. nr</th>
                        $csvcustomfields: {csvcustomfield |
                          <th>$csvcustomfield$</th>
                        }$
                      </tr>
                    </theader>
                    <tbody>
                      $csvpage.csvrows: {csvrow |
                        <tr$if(csvrow.isproblem)$ class="csvproblem"$endif$>
                          <td class="rownumbercol">$csvrow.rownumber$</td>
                          $csvrow.csvfields: { csvfield |
                            <td$if(csvfield.isproblem)$ class="csvproblem"$endif$>$csvfield.value$</td>
                          }$
                        </tr>
                      }$
                    </tbody>
                  </table>
                </div>
                <div class="csvpagefooter">
                  $if(!csvpage.isfirstcsvpage)$
                    <a class="btn-tiny black float-left previouscsvpage" href="#">
                      <div class="left"></div>
                      <div class="label">
                        <span class="btn-symbol black arrow-right"></span>
                        <span>Bakåt</span>
                      </div>
                      <div class="right"></div>
                    </a>
                  $endif$
                  $if(!csvpage.islastcsvpage)$
                    <a class="btn-tiny black float-right nextcsvpage" href="#">
                      <div class="left"></div>
                      <div class="label">
                        <span>Nästa</span>
                        <span class="btn-symbol black arrow-left"></span>
                      </div>
                      <div class="right"></div>
                    </a>
                  $endif$
                </div>
              </div>
            }$
          </div>
        </div>
      </div>
      <div class="modal-footer">
        
          <a class="cancel float-left" style="height:40px;">  
            <div class="label"> Välj fil </div> 
            <input class="multiFileInput submitOnUpload" style="top:-40px;right:0px;width:50px;" maxlength="1" type="file" name="csv" accept="application/csv" rel=".filelist"/>
          </a>
        
        <div class="filelist" style="display:none;"></div>
        <a class="btn-small green float-right close" href="#">
          <div class="left"></div>
          <div class="label"><p>Spara</p></div>
          <div class="right"></div>
        </a>
      </div>
    </div>
  </form>

  <form method="post" name="form" action="$linkissuedoc$"
        class="overlay redirectsubmitform" id="dialog-confirm-signinvite"
        rel=".stepForm">  
    <div class="modal-container">
      <div class="modal-header">
        <div class="modal-icon message"></div>
        <div class="modal-title">
          <h2 id="dialog-title-sign">Underteckna</h2>
        </div>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <p>Är du säker att du vill underteckna dokumentet <strong>$documenttitle$</strong>?</p>
          <p>När du undertecknat kommer en automatisk inbjudan att skickas till 
            <span class="Xinvited">Invited</span> med e-post.</p>
          <span class="eleghidden" style="display:none;">
            <h3>Välj typ av e-legitimation</h3>
            <p>Har du ingen e-legitimation? Du kan ladda ned en
              e-legitimation från din internetbank eller från
              Telia. <a href="http://www.e-legitimation.se/Elegitimation/Templates/LogolistPageTypeB.aspx?id=86"
                        target="_new">Klicka här</a> för att läsa mer.</p>
          </span>
        </div>
      </div>
      <div class="modal-footer">
        <input type="hidden" name="sign" value="automatic" id="signauto" />
        <a  class="close cancel" style="position:relative;top:6px;">Avbryt</a>
        <a href="#" class="btn-big blue float-right submiter signhidden">
          <div class="left"></div>
          <div class="label">
              <span>SkrivaPå</span>
              <span class="icon cross"></span>
          </div>
          <div class="right"></div>
        </a>
        <span class="eleghidden" style="display: none;">
          <input type="hidden" name="changetoeleg" id="elegprovider" value="" />
          <input type="hidden" name="signature" value="" id="signatureinput" />
          <input type="hidden" name="transactionid" value="" id="transactionidinput" />
          <a href="#" class="btn-small green float-right author bankid">
              <img src="/img/bankid.png" alt="BankID" />
          </a>
          <a href="#" class="btn-small green float-right author telia">
              <img src="/img/telia.png" alt="Telia Eleg" />
          </a>
          <a href="#" class="btn-small green float-right author nordea">
              <img src="/img/nordea.png" alt="Nordea Eleg" />
          </a>
        </span>
        
      </div>
    </div>
  </form>

  <form method="post" name="form" action="$linkissuedoc$" class="overlay redirectsubmitform" id="dialog-confirm-sendinvite" rel=".stepForm">  
    <div class="modal-container">
      <div class="modal-header">
        <div class="modal-icon message"></div>
        <div class="modal-title">
          <h2 id="dialog-title-sign">Skicka inbjudan</h2>
        </div>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <div id="dialog-confirm-text-send">
            <p>Du har valt en sekreterarroll och kommer själv inte att underteckna. Är du säker på att du vill skicka en inbjudan att underteckna dokumentet <strong>$documenttitle$</strong> till <span class="Xinvited">Invited</span>?</p>
          </div>

          <div id="dialog-confirm-text-send-fields">
            <p>Du har skapat fält för parterna att fylla i innan de undertecknar. Efter att alla har undertecknat kommer du att 
              få ett mail med en länk till en sida på SkrivaPå där du kan granska parternas uppgifter och underteckna dokumentet. </p>
            <p> Vill du skicka inbjudan att fylla i och underteckna dokumentet <strong>$documenttitle$</strong> till 
              <span class="Xinvited">Invited</span>?  </p>
          </div>

          <div id="dialog-confirm-text-send-normal">
            <p> Vill du skicka en inbjudan att underteckna dokumentet till <span class="Xinvited">Invited</span>? 
              Efter att de har undertecknat kommer du att få ett mail med en länk till en sida på SkrivaPå där du kan underteckna dokumentet.</p>
          </div>
        </div>
      </div>
      <div class="modal-footer">
        <input type="hidden" name="final" value="automatic" />
        <a  class="close cancel">Avbryt</a>
        <a href="#" class="btn-small green float-right submiter">
          <div class="left"></div>
          <div class="label">
            <span> Skicka</span>
            <div class="btn-symbol green arrow-left"></div></div>
          <div class="right"></div>
        </a>
      </div>
    </div>
  </form>

  <form method="post" name="form" action="" class="overlay" id="edit-invite-text-dialog" >
	<div class="modal-container">
	  <div class="modal-header">
		<div class="modal-icon message"></div>
		<div class="modal-title">
		  <h2>Hälsningsmeddelande</h2>
		</div>
		<a class="modal-close close"> </a>
	  </div>
	  <div class="modal-body">
		<div class="modal-content">
	      $invitationMailContent:noescape()$
		</div>
	  </div>
	  <div class="modal-footer">
		<a class="close cancel">Avbryt</a>
		<a class="editer message">Skriv eget meddelande</a>
		<a href="#" class="btn-small green float-right close" id="editing-invite-text-finished">
		  <div class="left"></div>
		  <div class="label">
			<p>Ok</p>
			<div class="btn-symbol green arrow-left"></div>
		  </div>
		  <div class="right"></div>
		</a>
	  </div>
	</div>
  </form>  

  <div class="hidden" id="hiddenttimestuffbox">
    <div id="daystosignbox">
    <span>
    Undertecknas inom
    <input class="signStepsBodyDaysLeftInput" type="text" id="daystosign" name="daystosign" value="$documentdaystosignboxvalue$" maxlength="2" size="2" autocomplete="off" /> 
    dagar
    </span>
    <div  class="datetodaystip signStepsBodyCalendar" rel="#daystosign"> </div> 
    </div>
  </div>

  <form method="post" action="$linkcancel$" class="overlay" id="cancel-by-author-dialog">
    <div class="modal-container">
      <div class="modal-header">
        <div class="modal-icon message"></div>
        <div class="modal-title">
          <h2> Återkalla inbjudan </h2>
          <p>Är du säker att du vill återkalla din inbjudan att underteckna dokumentet?
             <br/> När du återkallat inbjudan kommer nedanstående meddelande att skickas till dina motparter.
            
          </p>
        </div>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">     
          $cancelMailContent:noescape()$
        </div>
      </div>
      <div class="modal-footer">
        <a class="close cancel">Avbryt</a>
        <a class="editer message">Skriv eget meddelande</a>
        <a class="btn-small red float-right submiter">
          <div class="left"></div>
          <div class="label">
            <p>Återkalla inbjudan</p>
            <div class="btn-symbol red arrow-left"></div>
          </div>
          <div class="right"></div>
        </a>
      </div>
    </div>
    
  </form>
  <form method="post" name="form" action="$action$" id="dialog-confirm-sign-by-author" class="overlay">     
    <div class="modal-container">
      <div class="modal-header">
        <div class="modal-icon message"></div>
        <div class="modal-title">
          <h2>Underteckna</h2>   
        </div>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">     
          <p>Är du säker att du vill underteckna dokumentet <strong>$documenttitle$</strong>?</p>
          <p>När $partyUnsigned:noescape()$ undertecknat blir avtalet <strong>juridiskt bindande</strong> och
            det färdigställda avtalet skickas till din e-post.</p>
          <span class="eleghidden" style="display:none;">
            <h3>Välj typ av e-legitimation</h3>
            <p>Har du ingen e-legitimation? Du kan ladda ned en
              e-legitimation från din internetbank eller från
              Telia. <a href="http://www.e-legitimation.se/Elegitimation/Templates/LogolistPageTypeB.aspx?id=86"
                        target="_new">Klicka här</a> för att läsa mer.</p>
          </span>
        </div>
      </div>
      <div class="modal-footer">
        <input type="hidden" name="sign" value="automatic" id="signauto" />
        <a class="close cancel">Avbryt</a>
        <a href="#" class="btn-small green float-right submiter signhidden" id="editing-invite-text-finished">
          <div class="left"></div>
          <div class="label">
            <p>Underteckna </p>
            <div class="btn-symbol green arrow-left"></div>
          </div>
          <div class="right"></div>
        </a>
        <span class="eleghidden" style="display: none;">
          <input type="hidden" name="changetoeleg" id="elegprovider" value="" />
          <input type="hidden" name="signature" value="" id="signatureinput" />
          <input type="hidden" name="transactionid" value="" id="transactionidinput" />
          <a href="#" class="btn-small green float-right author2 bankid">
              <img src="/img/bankid.png" alt="BankID" />
          </a>
          <a href="#" class="btn-small green float-right author2 telia">
              <img src="/img/telia.png" alt="Telia Eleg" />
          </a>
          <a href="#" class="btn-small green float-right author2 nordea">
              <img src="/img/nordea.png" alt="Nordea Eleg" />
          </a>
        </span>

      </div>
    </div>
  </form>
  </div>

#####
####
pageDocumentForAuthorContent=
    $if(preparation)$
       $pageDocumentForAuthorContentPreparation()$
    $else$
       $pageDocumentForAuthorContentPostPreparation()$
    $endif$ 
#####
####
pageDocumentForAuthorContentPostPreparation=
  <div class="localdialogs">
    <div style="display: none">
      <script type="text/javascript"> 
        var documentid = "$documentid$", doctokenid = "$doctokenid$", doctokenhash = "$doctokenhash$";
      </script>
    </div>
  </div>
<div id="signStepsContainer"> 
  <div id="signStepsTitleRow"> 
     <span id="signStepsTitleRowTextContainer"> 
      <span class="title">DOKUMENT #$documentid$:</span>
      <span class="name">$documenttitle$</span>
     </span>
     <span class="download">
        <a href="$linkissuedocpdf$" target="_blank">
         Ladda ned PDF
       </a>
     </span>
  </div>
  <div id="signStepsBody" class="forauthor"> 
    <div id="signViewBodyLeft" > 
      <span id="signViewBodyLeftTextContainer"> 
        $documentinfotext:noescape()$
        $if(canberestarted)$
        <BR/><BR/>
        <form method="POST" action="/restart/$documentid$">
          <a class="btn-small red submit">
            <div class="left"></div>
            <div class="label">Starta om dokument</div> 
            <div class="right"></div>
        </a> 
        </form>
        $endif$
        $if(pending)$
        $if(!awaitingauthor)$
        $if(!timetosignset)$
        <a class="btn-small red centered" id="cancel"  name="cancel" rel="#cancel-by-author-dialog">
            <div class="left"></div>
            <div class="label">Återkalla inbjudan</div> 
            <div class="right"></div>
        </a>
        $endif$
        $endif$
        $endif$
      </span> 
    </div>
     $signatoriesBox()$
  </div>
</div>

<div id="documentBox">
  <script type="text/javascript">
    var docstate = $docstate:noescape()$;
  </script>

  <div class="waiting4page"></div>
</div>

$if(awaitingauthor)$
<script type="text/javascript" src="/js/bankid.js"> </script>
<div style="display:none;">
$validationdropdown()$
</div>
<div id="signViewBottomBox"> 
  <div id="signViewBottomBoxContainerLeft"> 
    <div id="signViewBottomBoxContainerLeftText"> 
      <input type="checkbox" id="signGuardCBox" class="signGuard" autocomplete="off" /><label for="signGuardCBox">Jag intygar att jag har granskat och förstått innehållet i dokumentet. Jag intygar även att samtliga parter är korrekta och att jag har befogenhet att skriva på.</label>
    </div>
  </div>

  <div id="signViewBottomBoxContainerRight"> 
    <div> 
      <input type="button" class="submit btn-blue center" value="SkrivaPå" id="signByAuthor" rel="#dialog-confirm-sign-by-author" name="sign" />
      <input class="cancel bigbutton" type="button"
             name="cancel" value="Återkalla inbjudan" id="cancel"
             rel="#cancel-by-author-dialog" />  
    </div>
  </div>
</div>
$endif$   
$signatoryScriptIfRequired()$
$pageDocumentForAuthorHelpersLocalDialogs()$
###
signatoryScriptIfRequired=   
$if(addSignatoryScript)$
      <script type="text/javascript" src="/js/signatory.js"></script>
$endif$
####
customfield=
<span class="customfield dragfield inputWrapper">
  <input type="text" class="customfieldbox" autocomplete="off" value="$otherFieldValue$" name="fieldvalue" infotext="$otherFieldName$" />
  <a href="#minus2" class="minus"></a>
  <img src="/theme/images/iconHand.png" class="draghandle" alt="drag" />
  <input type="hidden" value="$otherFieldID$" name="fieldid">
  <input type="hidden" value="$otherFieldOwner$" name="fieldsigid">
  <input type="hidden" value="$otherFieldName$" name="fieldname">
  <span class="type" style="display: none;">$otherFieldOwner$</span>
</span>
####
loadingdialog=
<div id="loadingdialog" class="overlay">
  <a href="#" id="loadinglink" style="display:none;" rel="#loadingdialog"></a>
  <div class="modal-container">
    <div class="modal-body" style="padding:30px">
      <center>
        <h3 id="loadingmessage">Loading</h3> 
        <img src="/theme/images/wait30trans.gif" style="margin:30px"/>
      </center>
    </div>
  </div>
</div>
####
