#####
pageDocumentDesign=
<div style="display: none">
  <script type="text/javascript"> 
    var documentid = "$documentid$";
  </script>
  <script type="text/javascript" src="/js/bankid.js"></script>
</div>
<div id="signStepsWrapper">

  <div id="signStepsContainer" class="follow">
    
    <div id="signStepsTitleRow">
      <span id="signStepsTitleRowTextContainer">
        $if(template)$
        <span class="title">MALL:</span>
        $elseif(offer)$
        <span class="title">OFFERT #$documentid$:</span>
        $elseif(contract)$
        <span class="title">DOKUMENT #$documentid$:</span>
        $endif$
        $docname()$
      </span>
    </div>
    
    <div id="signStepsMenu" class="signStepsMenu">
      <div id="step1select">
        <span class="step1select clickArea"></span>
        <span class="step1select step1icon"> </span>
        <span class="step1select text">
          $if(template)$
          Välj mall
          $elseif(contract)$
          Välj dokument
          $elseif(offer)$
          Välj offert
          $endif$
        </span>
      </div>
      
      <div id="step2select" class="$if(step2)$current$endif$">
        <a href="#" class="step2select clickArea"></a>
        <a href="#" class="step2select step2icon"> </a>
        <a href="#" class="step2select text">
          $if(template)$
          Utforma mall
          $elseif(basiccontract)$
          Fyll i uppgifter &amp; Skicka!
          $elseif(contract)$
          Fyll i uppgifter
          $elseif(offer)$
          Fyll i uppgifter &amp; Skicka!
          $endif$
        </a>  
      </div>
      $if(contract)$$if(!basiccontract)$
      <!-- Offers and basic contracts don't have step 3 -->
      <div id="step3select" class="$if(step3)$current$endif$">
        <a href="#" class="step3select clickArea"></a>
        <a href="#" class="step3select step3icon"> </a>
        <a href="#" class="step3select text">
          $if(template)$
          Färdigställ och spara
          $else$
          Underteckna och skicka!
          $endif$
        </a>
      </div>
      $endif$$endif$
      <div id="signStepsMenuEmptyStep">
        $if(!template)$$if(!basiccontract)$
        <div class="signStepsBodySave extraLink">
          <small><a class="submit" form=".stepForm" rel="template">SPARA SOM MALL</a></small>
        </div>
        $endif$$endif$

        $if(!template)$$if(contract)$
        $if(basiccontract)$
        <div class="signStepsFunctionalityChange  extraLink">
          <small><a id="toadvanced" class="switchIcon" rel="#dialog-confirm-advanced" style="padding-right:19px;">BYT TILL AVANCERAD PROCESS</a></small>
        </div>
        $else$
        <div class="signStepsFunctionalityChange extraLink">
          <small><a id="tobasic" class="switchIcon" rel="#dialog-confirm-basic">BYT TILL FÖRENKLAD PROCESS</a></small>
        </div>
        $endif$
        $endif$$endif$
      </div>
    </div>

    <div id="signStepsContentStack">

      <form action="" method="post" enctype="multipart/form-data" class="stepForm">
        <input type="hidden" name="allowedsignaturetypes"
               value="Email" />
        <input type="hidden" name="csvsigindex" value="$csvsigindex$"/>
        <input type="hidden" name="docfunctionality" value="$docfunctionality$"/>
        <input type="hidden" name="docname" value="$documenttitle$" />

        <div id="signStep2Content" $if(!step2)$ style="display: none" $endif$>
          <div id="signStepsBody">
            $if(!basiccontract)$
            <div id="signStepsBodyLeft">
              <div id="people">
                <div class="signStepsBodyTitle">
                  <span>PERSONER</span><span id="signorderlisttitle" class="float-right">SIGNERINGSORDNING</span>
                </div>
                
                <div id="peopleList" class="signStepsBodyForms">
                  <ol>
                  </ol>
                </div>
                
                <!--<div class="signStepsBodyAddField partCount">
                    <span>Visa</span>
                    <a href="">11-20</a>,
                    <a href="">21-30</a>
                </div>
                -->
              </div>
            </div>
            $endif$
            
            <div id="personpane">
              <span class="persondetails authordetails">
                <!-- 008 change/new -->
                <div class="signStepsBodyIcons">
                  <span class="partnumber">PART 1</span>
                  $if(!basiccontract)$
                  $if(contract)$
                  <a href="#man2" class="man"></a>
                  <a href="#" class="signingOrder"></a>
                  <select id="authorsignorder" class="float-right">
                    <option value="1">1</option>
                    <option value=""></option>
                  </select>
                  $endif$
                  <a class="plus" href="#"></a>
                  $endif$
                </div>
                
                <!-- 013 changed -->
                <div class="sigentry signStepsBodyForms $if(basiccontract)$dragDisabled$endif$" id="peopleDetails">
                  <span class="fields">
                    $if(authorfstname)$
                    <span class='dragtext authorfstname inputWrapper'>
                      <span style="display:none;" class="fieldid">fstname</span>
                      <span style="display:none;" class="fieldsigid">author</span>
                      <span style="display:none;" class="type">author</span>
                      <span class="fieldvalue">$authorfstname$</span>
                      $dragndropIcon()$
                    </span>
                    $endif$
                    $if(authorsndname)$
                    <span class='dragtext authorsndname inputWrapper'>
                      <span style="display:none;" class="fieldid">sndname</span>
                      <span style="display:none;" class="fieldsigid">author</span>
                      <span style="display:none;" class="type">author</span>
                      <span class="fieldvalue">$authorsndname$</span>
                      $dragndropIcon()$
                    </span>
                    $endif$
                    $if(authoremail)$
                    <span class='dragtext authoremail inputWrapper'>
                      <span style="display:none;" class="fieldid">email</span>
                      <span style="display:none;" class="fieldsigid">author</span>
                      <span style="display:none;"
                            class="type">author</span>
                      <span class="fieldvalue">$authoremail$</span>
                      $dragndropIcon()$
                    </span>
                    $endif$
                    $if(authorcompany)$
                    <span class='dragtext authorcomp inputWrapper'>
                      <span style="display:none;" class="fieldid">company</span>
                      <span style="display:none;" class="fieldsigid">author</span>
                      <span style="display:none;" class="type">sig</span>
                      
                      <span class="fieldvalue">$authorcompany$</span>
                      $dragndropIcon()$
                    </span>
                    $endif$
                    $if(authorpersonnumber)$
                    <span class='dragtext authorpersnum inputWrapper'>
                      <span style="display:none;" class="fieldid">personalnumber</span>
                      <span style="display:none;" class="fieldsigid">author</span>
                      <span style="display:none;"
                            class="type">sig</span>
                      <span class="fieldvalue">$authorpersonnumber$</span>
                      $dragndropIcon()$
                    </span>
                    $endif$
                    $if(authorcompanynumber)$
                    <span class='dragtext authorcompnum inputWrapper'>
                      <span style="display:none;" class="fieldid">companynumber</span>
                      <span style="display:none;" class="fieldsigid">author</span>
                      <span style="display:none;"
                            class="type">sig</span>
                      <span class="fieldvalue">$authorcompanynumber$</span>
                      $dragndropIcon()$
                    </span>
                    $endif$
                    <div class="otherfields">
                      $authorOtherFields:noescape()$
                    </div>
                  </span>
                  <div class="partyrole">
                    <a href="#" class="popup-close closelink"></a>
                    <input type="radio" name="authorrole" id="authorsignatoryradio" value="signatory"><span class="radiolabel">Undertecknande part</span></input>
                    <br/>
                    <input type="radio" name="authorrole" id="authorsecretaryradio" value="secretary"><span class="radiolabel">Ej undertecknande part</span></input>
                  </div>                  
                </div>
              </span>

            </div>
            
            <div id="signStepsBodyFunctions">
              $if(!basiccontract)$
              $if(contract)$
              <div id="signStepsBodyFunctionsBox">
                <ul>
                  <li class="plus"><a class="designFunctionalLink" href="#add"  id="addSignatory">LÄGG TILL PERSON</a></li>
                  <li class="minus"><a class="designFunctionalLink" href="#remove"  id="delSignatory">TA BORT PERSON</a></li>
                </ul>
                <div class="signStepsButton" id="signStepsNextButton" >
                  <a href="#nextStep" id="nextstep" class="btn-small green center" style="display:inline-block">
                    <div class="left"></div>
                    <div class="label">
                      <p style="display:inline-block;width:80px;">Nästa steg</p>
                      <span class="btn-symbol green arrow-left"></span>
                    </div>
                    <div class="right"></div>
                  </a>  
                </div>
              </div> 
              $elseif(offer)$
              <div id="signStepsBodyFunctionsBox" style="padding-left:17px;">
                <div>
                  <input style="position:relative;top:2px;left:2px;margin-right:5px;" type="checkbox" class="addremovecheckbox flashOnOn" flashtype="red"
                         rel="#daystosignbox"
                         location="#datetosigncontainer"
                         oldlocation="#hiddenttimestuffbox"
                         autocomplete="off"
                         value="$if(timetosignset)$on$else$off$endif$"
                         id="daystosigncheckbox" />
                  <span class="hidden flashMessage">Observera! Du kan ej återkalla offerten om du väljer ett förfallodatum. Detta regleras av avtalslagen.</span>
                  <label for="daystosigncheckbox">Förfallodatum</label>
                  <div id="datetosigncontainer" $if(contract)$ style="padding-left:20px" $endif$>  </div>
                </div>

                <div class="signStepsBodyInvation">
                  <small><a rel="#edit-invite-text-dialog" id="editinvitetextlink" href="#" style="padding-top:3px">HÄLSNINGSMEDDELANDE</a></small>
                  <input type="hidden" id="invitetext" name="invitetext" value="$documentinvitetext$" />
                </div>
                
                $if(template)$
                <div class="signStepButton" style="display:inline-block;padding-top:4px;">
                  <a class="btn-small green center submiter">
                    <div class="left"></div>
                    <div class="label">
                      <span>Spara mall</span>
                      <span class="btn-symbol green arrow-left"></span>
                    </div>
                    <div class="right"></div>
                  </a>
                </div>
                $else$
                <input type="hidden" value="email" name="validationmethod" />
                <div class="signStepButton" style="display:inline-block;padding-top:4px;">
                  <a type="button" id="sendinvite" rel="#dialog-confirm-sendinvite"  class="btn-small green center">
                    <div class="left"></div>
                    <div class="label" style=""><span > Skicka offert</span>
                      <span class="btn-symbol green arrow-left" ></span></div> 
                    <div class="right"></div>
                  </a>
                </div>
                $endif$
              </div>
              $endif$
              $elseif(basiccontract)$
              <div id="signStepsBodyFunctionsBox">
                <div>
                  <div style="padding-bottom:2px;padding-left:1px;">Välj verifieringsmetod</div>
                  <select name="validationmethod"
                          id="validationdropdown">
                    <option value="email" $if(emailselected)$selected$endif$>E-post</option>
                    <option value="eleg" $if(elegselected)$selected$endif$>E-Legitimation</option>
                  </select>
                </div>
                <div class="signStepsBodyInvation" style="padding-top:7px;padding-bottom:9px;" class="center">
                  <small><a rel="#edit-invite-text-dialog" id="editinvitetextlink" href="#" style="padding-top:3px;">HÄLSNINGSMEDDELANDE</a></small>
                  <input type="hidden" id="invitetext" name="invitetext" value="$documentinvitetext$" />
                </div>
                <div class="signbutton" style="display:inline-block;">
                  <a type="button" id="signinvite" rel="#dialog-confirm-signinvite" class="btn-small blue center">
                    <div class="left"></div>
                    <div class="label" style="width:134px;">
                      <span style="padding-left:9px;padding-right:9px">SkrivaPå</span>
                      <span class="btn-symbol cross"></span>
                    </div> 
                    <div class="right"></div>
                  </a>
                </div>
              </div>
              $endif$
            </div>
          </div>
        </div>  

        $if(!basiccontract)$$if(contract)$
        <div id="signStep3Content" $if(!step3)$ style="display: none" $endif$>

          <div id="signStepsBody">
            
            <div id="signStepsBodyLeft" class="step3">
              <div class="options">
                <p>
                  <span>
                    <input style="position:relative;top:2px" 
                           type="checkbox" 
                           class="addremovecheckbox flashOnOn" 
                           flashtype="red"
                           rel="#daystosignbox"
                           location="#datetosigncontainer"
                           oldlocation="#hiddenttimestuffbox"
                           autocomplete="off"
                           value="$if(timetosignset)$on$else$off$endif$"
                           id="daystosigncheckbox" />
                    <span class="hidden flashMessage">Varning: Om du väljer ett förfallodatum kan du inte återkalla inbjudan innan datumet förfallit. Detta regleras av avtalslagen.</span>
                    <label for="daystosigncheckbox">Aktivera förfallodatum</label>
                    <div id="datetosigncontainer" style="padding-left:20px">  </div>
                  </span>
                </p>
                
                
                <!-- 
                     <p class="signStepsBodySaveMethods">
                       <label for="c">Lagring</label> 
                       <select id="c">
                         <option value="1">Standard</option>
                         <option value="2">Trustweaver</option>
                         
                         <option value="3">Option 3</option>
                       </select> 
                     </p>
                     -->
              </div>
              <div class="signStepsBodyInvation">
                <small><a rel="#edit-invite-text-dialog" id="editinvitetextlink" href="#" style="padding-top:3px">JUSTERA HÄLSNINGSMEDDELANDE</a></small>
                <input type="hidden" id="invitetext" name="invitetext" value="$documentinvitetext$" />
              </div>
              <div class="signStepsBodyAttachments">
                <small>
                  <a rel="#update-attachments-dialog" href="#"
                     style="padding-top:3px" id="addattachmentlink">BIFOGA BILAGA</a>
                  $if(isattachments)$
                  <span class="tooltip" rel="#tooltip-attachmentlist">($attachmentcount$ dokument)</span>
                  <div id="tooltip-attachmentlist" class="hidden">
                    $attachments:{attachment |
                    <div>$attachment.attachmentname$</div>
                    }$
                  </div>
                  $endif$
                </small>
              </div>
              <div class="signStepsBodySigAttachments">
                <small>
                  <a rel="#update-sigattachments-dialog" href="#"
                     style="padding-top:3px" id="addsigattachmentlink">BEGÄR BILAGA</a>
                </small>
              </div>
              
            </div>
            
            <div class="border"></div>   
            <div id="signStepsBodyRight" class="step3">      
              <!-- 015 new -->
            </div>
            <div class="border"></div>

            <div id="signStepsBodyFunctions" class="step3">
              <div class="signStepsBodyFunctionsBox options">
                <!--
                    <p>
                      <label for="d">Metod</label> 
                      <select id="d">
                        <option value="1">e-post</option>
                        <option value="2">e-legitimation</option>
                        <option value="3">Option 3</option>
                      </select>
                    </p>
                    -->
                <!-- 013 changed sign button tag -->
                
                
              </div>
              $if(template)$
              <a class="btn-small green center submiter saveTemplate">
                <div class="left"></div>
                <div class="label">
                  <p>Spara mall</p>
                  <span class="btn-symbol green arrow-left"></span>
                </div>
                <div class="right"></div>
              </a>
              $else$
              <div> 
                $validationdropdown()$
                <div style="padding-bottom:8px; padding-top: 2px;">
                  <input id="switchercheckbox" style="position:relative;top:2px" type="checkbox" class="sendcheckbox" on=".sendbutton" off=".signbutton" autocomplete="off" value="off" > 
                  <label for="switchercheckbox" class="usual" style="margin-left:3px;">Underteckna sist </label>
                  <label class="secretary" style="margin-left:3px;display:none;">Ej undertecknande part</label>
            </input>
                </div>
                <div class="sendbutton" style="display:none">
                  <a type="button" id="sendinvite" rel="#dialog-confirm-sendinvite"  class="btn-big green center">
                    <div class="left"></div>
                    <div class="label" style="width:120px">
		              <span>Skicka </span>
                      <span class="btn-symbol green arrow-left"
                            style="position:relative;top:3px;right:0px"></span>
	                </div> 
                    <div class="right"></div>
                  </a>
                </div>
                <div class="signbutton">
                  <a type="button"  id="signinvite" rel="#dialog-confirm-signinvite" class="btn-big blue center" style="float:none">
                    <div class="left"></div>
                    <div class="label" style="width:120px">
                      <span>SkrivaPå </span>
                      <span class="icon cross"></span>
                    </div> 
                    <div class="right"></div>
                  </a>
                </div>
              </div>
              $endif$

            </div>
            
          </div>            
        </div>
        $endif$$endif$
      </form>
    </div>
  </div>
  $pageDocumentForAuthorHelpersLocalDialogs()$
  
</div>      
</div>  
<div id="documentBox">
  <script type="text/javascript">
    var signingOrderEnabled = false;
    var allNonSignatories = false;
    var docstate = $docstate:noescape()$;
    var csvrowcount = $csvrowcount$;
    var initialperson = $initialperson$;
    var isaftercsvupload = $if(isaftercsvupload)$true$else$false$endif$;
    var offer = $if(offer)$true$else$false$endif$;
  </script>
  <script src="/js/templates.js" type="text/javascript"></script>
  <div class="waiting4page"></div>
</div>
<div id="templates" style="display:none;">
  <span class="customfield dragfield inputWrapper">
    <input type="text" value="" class="customfieldbox" autocomplete="off" />
    <a href="#minus2" class="minus"></a>
    <img src="/theme/images/iconHand.png" class="draghandle" alt="drag" />
  </span>
  <span class="csvfileinfo"><a href="#" class="csvinfo$if(!isvalidcsv)$ csvinvalid$endif$" rel="#dialog-csvupload-2">Granska data</a></span>
  <span class="persondetails">
    
    <!-- 008 change/new -->
    <div class="signStepsBodyIcons">
      <span class="partnumber">PART 1</span>
      $if(!basiccontract)$
        $if(contract)$
        <a href="#" class="man"></a>
        <a href="#" class="csv single" rel="#dialog-csvupload-1"></a>
        <a href="#" class="csv multi" rel="#dialog-csvupload-2" style="display:none;"></a>
        <a href="#" class="signingOrder"></a>
        <a href="#" class="plus"></a>
        $elseif(offer)$
        <input type="hidden" name="signatoryrole" value="signatory">
        <a href="#" class="plus"></a>
        $endif$
        <select class="signorder float-right" name="signatorysignorder"></select>
      $endif$
    </div>
    
    <!-- 013 changed -->
    <div class="sigentry signStepsBodyForms $if(basiccontract)$dragDisabled$endif$" id="peopleDetails">

      <input type="hidden" name="sigid" value="" />
      <span class="fields">
        <span class='dragfield sigfstname inputWrapper'>
          <span style="display:none;" class="fieldid">fstname</span>
          <span style="display:none;" class="type">author</span>
          <input type="text" infotext="Förnamn"
                 name="signatoryfstname" autocomplete="off" value="" />
          $dragndropIcon()$
        </span>
        <span class='dragfield sigsndname inputWrapper'>
          <span style="display:none;" class="fieldid">sndname</span>
          <span style="display:none;" class="type">author</span>
          <input type="text" infotext="Efternamn"
                 name="signatorysndname" autocomplete="off" value="" />
          $dragndropIcon()$
        </span>
        <span class='dragfield sigemail inputWrapper'>
          <span style="display:none;" class="fieldid">email</span>
          <span style="display:none;" class="type">author</span>
          <input type="email" infotext="E-post" name="signatoryemail" autocomplete="off" />
          $dragndropIcon()$
        </span>
        <span class='dragfield sigcomp inputWrapper'>
          <span style="display:none;" class="fieldid">company</span>
          <span style="display:none;" class="type">sig</span>
          <input type="text" infotext="Företag" name="signatorycompany" autocomplete="off" />
          $dragndropIcon()$
        </span>
        <span class='dragfield sigpersnum inputWrapper'>
          <span style="display:none;" class="fieldid">personalnumber</span>
          <span style="display:none;" class="type">sig</span>
          <input type="text" infotext="Pers. nr" name="signatorypersonalnumber" autocomplete="off" />
          $dragndropIcon()$
        </span>
        <span class='dragfield sigcompnum inputWrapper'>
          <span style="display:none;" class="fieldid">companynumber</span>
          <span style="display:none;" class="type">sig</span>
          <input type="text" infotext="Org. nr" name="signatorycompanynumber" autocomplete="off" />
          $dragndropIcon()$
        </span>
      </span>
      <div class="otherfields">
      </div>
      <div class="partyrole">
        <a href="#" class="popup-close closelink"></a>
        <input type="hidden" name="signatoryrole" value="signatory"> </input>
        <input type="radio" class="sigrole_signatory" value="signatory"><label class="radiolabel">Undertecknande part</label></input>
        <br/>
        <input type="radio" class="sigrole_nonsignatory" value="nonsignatory"><label class="radiolabel">Ej undertecknande part</label></input>
      </div>
    </div>
  </span>
</div>
####
docname=
       <span class="docname">
         <span class="docname-display">
           $if(!basiccontract)$
           <a href="#" class="icon edit" style="margin-right: 2px"></a>
           $endif$
           <span class="visible-docname">$documenttitle$</span>
         </span>
         <span class="docname-edit" style="display:none;">
           <a href="#" class="icon small ok" style="margin-right: 2px; float: none"></a>
           <input type="text" name="docname-edit" value="$documenttitle$" />
         </span>
       </span>
####
dragndropIcon=$if(!basiccontract)$<img src="/theme/images/iconHand.png" class="draghandle" alt="drag" />$endif$
#####
validationdropdown= 
<div>
  <span>Välj verifieringsmetod</span>
  <select name="validationmethod"
          id="validationdropdown">
    <option value="email" $if(emailselected)$selected$endif$>E-post</option>
    <option value="eleg" $if(elegselected)$selected$endif$>E-Legitimation</option>
  </select>
</div> 
#####
emailhidden=
<input type="hidden" value="email" name="validationmethod" />
####
pageDocumentForAuthorHelpersLocalDialogs=
<div class="localdialogs">
  $loadingdialog()$

    <form method="post" name="form" action="" class="overlay redirectsubmitform" id="update-sigattachments-dialog" rel=".stepForm">
      <input type="hidden" name="sigattachments" value="true" />
      
      <div class="modal-container">
      <div class="modal-header">
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h2>Begär bilaga</h2>
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <div>     
            <a href="#" class="plus" >  </a>Lägg till bilaga
          </div>
          <table>
            <tr><td></td>
              <td>Bilaga</td>
              <td>Beskrivning</td>
              <td>Begar fran</td>
            </tr>
            $sigattachments:{a|
            <tr><td><a href="#" class="minus">  </a></td>
              <td><input type="text" name="sigattachname"
              value="$a.attachmentname$" /></td>
              <td><textarea name="sigattachdesc" value="$a.attachmentdescription$" /></td>
              <td><select id="signatoryselector"><option selected>Mottagare</option>
              </select></td>
              <td><ul id="selectedsigs">
                  $a.signatories:{s|
                  <li>$s.signame$ <a href="#"
                  class="remove">x</a><span style="display:none">$s.email$</span></li>
                  }$
                  <input type="hidden" name="sigattachemails" value="$a.sigemails$" />
              </ul></td>
            </tr>}$
          </table>
        </div>
      </div>
      <div class="modal-footer">
        <div class="float-left">
          <a class="close cancel">Avbryt</a>
        </div>
        <a class="btn-small green float-right submiter">
          <div class="left"></div>
          <div class="label">
            <p>
              Spara
            </p>
            <div class="btn-symbol green arrow-left"></div>
          </div>
          <div class="right"></div>
        </a>
      </div>
    </div>
  </form>

  <form method="post" name="form" action="" class="overlay redirectsubmitform" id="update-attachments-dialog" rel=".stepForm">
    <input type="hidden" name="updateattachments" value="true" />
    <div class="modal-container">
      <div class="modal-header">
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h2>Välj bilagor</h2>
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <div>     
            Välj de bilagor du vill bifoga
          </div>
          <span id="attachmentbuttons">
            <div class="signStepsBodyUploadBox">
              <div>
                <span id="signStepsBodyUploadTextContainer"> 
                  <span class="header">Välj fil från dator</span><br/>
                  <span class="text">OBS! Endast PDF</span> 
                </span>
              </div>
              <br/>
              <div class="signStepsButtonContainer">
                <a class="btn-small green">  
                  <div class="left"> </div>
                  <div class="label" style="width:80px">Välj fil</div> 
                  <div class="right"> </div>
                  <input class="multiFileInput" style="top:-40px;right:0px" maxlength="20" type="file" name="attachment" accept="application/pdf" rel="#tobeattached" />
                </a>
              </div>
            </div>
            <div class="signStepsBodyUploadBox">
              <div>
                <span id="signStepsBodyUploadTextContainer"> 
                  <span class="header">Välj bilaga</span><br/>
                  <span class="text">Sparad på SkrivaPå</span> 
                </span>
              </div>
              <br/>
              <div class="signStepsButtonContainer">
                <a id="showexistingattachments" class="btn-small green">  
                  <div class="left"> </div>
                  <div class="label" style="width:80px">Välj bilaga</div> 
                  <div class="right"> </div>
                </a>
              </div>
            </div>
          </span>
          <span id="attachmentselectlist" style="display:none">
            <div class="existingattachments">
              <ul>
                $existingattachments:{a |
                <li><a href="#" class="addattachment" rel="$a.attachmentid$">$a.attachmentname$</a></li>
                }$
              </ul>
            </div>
          </span>
          <div id="alreadyattached">
            $attachments:{attachment |
              <div class="alreadyattacheditem">
                <div class="display">
                  <a href="#" class="remove">x</a><span style="padding-left:5px;">$attachment.attachmentname$</span>
                </div>
                <div class="data">
                  <input type="hidden" name="attachmentid" value="$attachment.attachmentid$" />
                  <input type="hidden" name="removeattachment" value="false" />
                </div>
              </div>
            }$
          </div>
          <div id="tobeattached">
          </div>
        </div>
      </div>
      <div class="modal-footer">
        <div class="float-left">
          <a class="close cancel">Avbryt</a>
        </div>
        <a class="btn-small green float-right submitAttachments">
          <div class="left"></div>
          <div class="label">
            <p>
              Bifoga
            </p>
            <div class="btn-symbol green arrow-left"></div>
          </div>
          <div class="right"></div>
        </a>
      </div>
    </div>
  </form>

  <form method="post" name="form" action="" class="overlay redirectsubmitform" id="dialog-csvupload-1" rel=".stepForm">
    <input type="hidden" name="csvupload" value="true" />
    <div class="modal-container">
      <div class="modal-header">
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h2>Massutskick</h2>
        </span>
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
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h2>Massutskick: Granska Data</h2>
        </span>
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
          <div class="filelist" style="display:none;"></div>
        </a>
        
        <a class="btn-small green float-right close" href="#">
          <div class="left"></div>
          <div class="label"><p>Spara</p></div>
          <div class="right"></div>
        </a>
      </div>
    </div>
  </form>

  <form method="post" name="form" action="$linkissuedoc$" class="overlay redirectsubmitform" id="dialog-confirm-advanced" rel=".stepForm">
    <input type="hidden" name="changefunctionality" value="true" />
    <input type="hidden" name="toadvanced" value="true" />
    <div class="modal-container">
      <div class="modal-header">
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h2 id="dialog-title">Vill du byta till avancerad process?</h2>
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <p>Du kan byta till avancerad process för att ta del av mer avancerade funktioner så som förfallodatum, mallar, massutskick mm. Denna process tillhör betalplanerna Business och Enterprise, även vid fri testperiod.</p>
        </div>
      </div>
      <div class="modal-footer">
        <a  class="close cancel float-left">Avbryt</a>
        <a href="#" class="btn-small green float-right submiter toadvanced">
          <div class="left"></div>
          <div class="label">
            <span>Byt process</span>
          </div>
          <div class="right"></div>
        </a>
      </div>
    </div>
  </form>

  <form method="post" name="form" action="$linkissuedoc$" class="overlay redirectsubmitform" id="dialog-confirm-basic" rel=".stepForm">
    <input type="hidden" name="changefunctionality" value="true" />
    <input type="hidden" name="tobasic" value="true" />
    <div class="modal-container">
      <div class="modal-header">
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h2 id="dialog-title">Vill du byta till förenklad process?</h2>
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
         <p>
            Du kan byta till förenklad process för att göra processen ännu smidigare.
            Vid betalplan Business eller Enterprise kan du givetvis byta tillbaks när du vill, även vid fri testperiod. 
          </p>
          <p> 
            När du byter kommer alla ändringar du gjort förutom de för part 1 och part 2 att försvinna. 
            Är du säker att du vill byta process
          </p>    
     </div>
      </div>
      <div class="modal-footer">
        <a  class="close cancel float-left">Avbryt</a>
        <a href="#" class="btn-small green float-right submiter tobasic">
          <div class="left"></div>
          <div class="label">
            <span>Byt process</span>
          </div>
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
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h2 id="dialog-title-sign">Underteckna</h2>
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <p>Är du säker att du vill underteckna dokumentet <strong class="visible-docname">$documenttitle$</strong>?</p>
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
        <a class="close cancel float-left">Avbryt</a>
        <a href="#" class="btn-small blue float-right submiter signhidden">
          <div class="left"></div>
          <div class="label">
            <p>SkrivaPå</p>
            <span class="btn-symbol cross"></span>
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
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h2 id="dialog-title-sign">
               $if(contract)$
                Skicka inbjudan
               $elseif(offer)$
                Skicka offert
               $endif$
          </h2>
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
         $if(contract)$ 
          <div id="dialog-confirm-text-send">
            <p>Du har valt en sekreterarroll och kommer själv inte att underteckna. Är du säker på att du vill skicka en inbjudan att underteckna dokumentet <strong class="visible-docname">$documenttitle$</strong> till <span class="Xinvited">Invited</span>?</p>
          </div>
          <div id="dialog-confirm-text-send-fields">
            <p>Du har skapat fält för parterna att fylla i innan de undertecknar. Efter att alla har undertecknat kommer du att 
              få ett mail med en länk till en sida på SkrivaPå där du kan granska parternas uppgifter och underteckna dokumentet. </p>
            <p> Vill du skicka inbjudan att fylla i och underteckna dokumentet <strong class="visible-docname">$documenttitle$</strong> till 
              <span class="Xinvited">Invited</span>?  </p>
          </div>

          <div id="dialog-confirm-text-send-normal">
            <p> Vill du skicka en inbjudan att underteckna dokumentet till <span class="Xinvited">Invited</span>? 
              Efter att de har undertecknat kommer du att få ett mail med en länk till en sida på SkrivaPå där du kan underteckna dokumentet.</p>
          </div>
          $elseif(offer)$
            <div id="dialog-confirm-text-send">
              <p>Är du säker på att du vill skicka offerten <strong class="visible-docname">$documenttitle$</strong> till <span class="Xinvited">Invited</span>?</p>
            </div>
          $endif$
        </div>
      </div>
      <div class="modal-footer">
        <input type="hidden" name="final" value="automatic" />
        <a  class="close cancel float-left">Avbryt</a>
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
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h2>Hälsningsmeddelande</h2>
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content disablelinks">
          $invitationMailContent:noescape()$
        </div>
      </div>
      <div class="modal-footer">
            <div class="float-left">
        <a class="close cancel">Avbryt</a>
        <a class="editer message">Skriv eget meddelande</a>
            </div>
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

    $if(contract)$
         <span> 
            Undertecknas inom   <input class="signStepsBodyDaysLeftInput" type="text" id="daystosign" name="daystosign" value="$documentdaystosignboxvalue$" maxlength="2" size="2" autocomplete="off" />     dagar
         </span>
        <div  class="datetodaystip signStepsBodyCalendar" rel="#daystosign"> </div> 
    $elseif(offer)$
         <div  class="datetodaystip signStepsBodyCalendar" style="width:13px;" rel="#daystosign"> </div>
         <span> 
            Bekräftas inom   <input class="signStepsBodyDaysLeftInput" type="text" id="daystosign" name="daystosign" value="$documentdaystosignboxvalue$" maxlength="2" size="2" autocomplete="off" />  dagar
         </span>
    $endif$
  
    </div>
  </div>
  </div>
#####
