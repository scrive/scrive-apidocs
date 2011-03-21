#####
pageDocumentForAuthorContentPreparation=
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

       $if(contract)$
           <span class="title">DOKUMENT #$documentid$:</span>
           <span class="name"> $documenttitle$</span> 
       $elseif(template)$
           <span class="title">MALL:</span>
           <span class="name"> $documenttitle$</span> 
       $endif$
    
    <!-- <span class="changeName"><a href="#changeName">BYT TITEL</a></span> -->

      </span>
$if(contract)$
<!-- <span style="float: right; display: inline-block; margin: 8px 10px 0pt 18px;" ><a class="submit title" form=".stepForm" >SPARA SOM UTKAST</a></span> 
<span style="float: right; display: inline-block; margin: 6px 10px 0pt 18px;font-weight:bold;" ><a class="submit title" style="color:#999999" form=".stepForm" rel="template" >SPARA SOM MALL</a></span>  -->
$elseif(template)$
$endif$  

      <!-- 
         <div class="signStepsButton" id="signStepsNextButton">
           <a href="#nextStep" class="clickArea"></a>
           
           <a href="#nextStep"><span class="icon arrowRight"></span></a><a href="#nextStep">
             <span class="text">Nästa steg</span>
           </a>
         </div>
         -->
    </div>
    
    <div id="signStepsMenu">
      <div id="step1select">
        <span class="step1select clickArea"></span>
        <span class="step1select step1icon"> </span>
        <span class="step1select text">$if(contract)$Välj dokument$elseif(template)$Välj mall$endif$</span>
      </div>
      
      <div id="step2select" class="$if(step2)$current$endif$">
        <a href="#" class="step2select clickArea"></a>
        <a href="#" class="step2select step2icon"> </a>
        <a href="#" class="step2select text">$if(contract)$Fyll i uppgifter$elseif(template)$Utforma mall$endif$</a>
      </div>
      
      <div id="step3select" class="$if(step3)$current$endif$">
        <a href="#" class="step3select clickArea"></a>
        <a href="#" class="step3select step3icon"> </a>
        <a href="#" class="step3select text">$if(contract)$Underteckna och skicka!$elseif(template)$Färdigställ och spara$endif$</a>
      </div>
      
      <div id="signStepsMenuEmptyStep"></div>
    </div>

    <div id="signStepsContentStack">

      <form action="" method="post" class="stepForm">
        <input type="hidden" name="allowedsignaturetypes"
               value="Email" />

        <div id="signStep2Content" $if(!step2)$ style="display: none" $endif$>
          <div id="signStepsBody">
            
            <div id="signStepsBodyLeft">
              <div id="people">
                <div class="signStepsBodyTitle">
                  <span>PERSONER</span>
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
            
            <div id="personpane">
              <span class="persondetails authordetails">
                <!-- 008 change/new -->
                <div class="signStepsBodyIcons">
                  <span class="partnumber">PART 1</span>
                  <a href="#man2" class="man"></a>
                  <a class="plus" href="#"></a>
                </div>
                
                <!-- 013 changed -->
                <div class="sigentry signStepsBodyForms" id="peopleDetails">
                  <span class="fields">
                    $if(authorfstname)$
                    <span class='dragtext authorfstname inputWrapper'>
                      <span style="display:none;" class="fieldid">fstname</span>
                      <span style="display:none;" class="fieldsigid">author</span>
                      <span style="display:none;" class="type">author</span>
                      <span class="fieldvalue">$authorfstname$</span>
                      <img src="/theme/images/iconHand.png" class="draghandle"
                           alt="drag" />
                    </span>
                    $endif$
                    $if(authorsndname)$
                    <span class='dragtext authorsndname inputWrapper'>
                      <span style="display:none;" class="fieldid">sndname</span>
                      <span style="display:none;" class="fieldsigid">author</span>
                      <span style="display:none;" class="type">author</span>
                      <span class="fieldvalue">$authorsndname$</span>
                      <img src="/theme/images/iconHand.png" class="draghandle"
                           alt="drag" />
                    </span>
                    $endif$
                    $if(authoremail)$
                    <span class='dragtext authoremail inputWrapper'>
                      <span style="display:none;" class="fieldid">email</span>
                      <span style="display:none;" class="fieldsigid">author</span>
                      <span style="display:none;"
                            class="type">author</span>
                      <span class="fieldvalue">$authoremail$</span>
                      <img src="/theme/images/iconHand.png" class="draghandle"
                           alt="drag" />
                    </span>
                    $endif$
                    $if(authorcompany)$
                    <span class='dragtext authorcomp inputWrapper'>
                      <span style="display:none;" class="fieldid">company</span>
                      <span style="display:none;" class="fieldsigid">author</span>
                      <span style="display:none;" class="type">sig</span>
                      
                      <span class="fieldvalue">$authorcompany$</span>
                      <img src="/theme/images/iconHand.png" class="draghandle"
                           alt="drag" />
                    </span>
                    $endif$
                    $if(authorcompanynumber)$
                    <span class='dragtext authornum inputWrapper'>
                      <span style="display:none;" class="fieldid">number</span>
                      <span style="display:none;" class="fieldsigid">author</span>
                      <span style="display:none;"
                            class="type">sig</span>
                      <span class="fieldvalue">$authorcompanynumber$</span>
                      <img src="/theme/images/iconHand.png" class="draghandle" 
                           alt="drag" />
                    </span>
                    $endif$
                    <div class="otherfields">
                      $authorOtherFields:noescape()$
                    </div>
                  </span>
                  <div class="partyrole">
                    <a href="#" class="closelink"><small>close</small></a>
                    <input type="radio" name="authorrole" id="authorsignatoryradio" value="signatory" checked>Undertecknare</input>
                    <br/>
                    <input type="radio" name="authorrole" id="authorsecretaryradio" value="secretary">Sekreterare</input>
                  </div>
                  
                </div>
              </span>

            </div>
            
            <div id="signStepsBodyFunctions">
              <div id="signStepsBodyFunctionsBox">
                <ul>

                  <li class="plus"><a class="designFunctionalLink" href="#add"  id="addSignatory">LÄGG TILL PERSON</a></li>
                  <li class="minus"><a class="designFunctionalLink" href="#remove"  id="delSignatory">TA BORT PERSON</a></li>

                </ul>
                <div class="signStepsButton" id="signStepsNextButton" >
                  <a href="#nextStep" id="nextstep" class="new-btn-small green2 center"><p>Nästa steg</p></a>  
                </div>
              </div>
            </div>
          </div>
        </div>
        
        <div id="signStep3Content" $if(!step3)$ style="display: none" $endif$>
          <div id="signStepsBody">
            
            <div id="signStepsBodyLeft" class="step3">
              <div class="options">
                <p>
                  <span>
                    <input style="position:relative;top:2px" 
                           type="checkbox" 
                           class="addremovecheckbox
                                  flashOnOn" 
                           rel="#daystosignbox"
                           location="#datetosigncontainer"
                           oldlocation="#hiddenttimestuffbox"
                           autocomplete="off"
                           value="$if(timetosignset)$on$else$off$endif$"
                           id="daystosigncheckbox" />
                    <span class="hidden flashMessage">Varning: Om du väljer ett förfallodatum kan du inte återkalla inbjudan innan datumet förfallit. Detta regleras av avtalslagen.</span>
                    <label for="daystosigncheckbox">Aktivera förfallodatum</label>
                    <div id="datetosigncontainer" style="padding-left:15px">  </div>
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
             <div class="signStepsBodySave">
                <small><a class="submit title" form=".stepForm" rel="template" >SPARA SOM MALL</a></small>
             </div>
              $validationdropdown()$
        </div>
        
        <div class="border"></div>   
        <div id="signStepsBodyRight" class="step3">
          $if(contract)$
          <input type="button" class="submit btn-blue center" id="signinvite" rel="#dialog-confirm-signinvite" value="SkrivaPå" name="final" />
          $endif$
        
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
          $if(contract)$
            <a type="button" id="sendinvite" rel="#dialog-confirm-sendinvite" name="final"  class="new-btn-small green2 center">
              <p>Skicka </p> 
            </a>

          $elseif(template)$
            <a type="submit" name="save" class="submit new-btn-small green2 center"> 
              <p>Spara mall</p> 
            </a>
           $endif$

            </div>
            
          </div>            
        </div>
      </form>
    </div>
  </div>
  $pageDocumentForAuthorHelpersLocalDialogs()$
  <!-- span>signStepsBorderBottom går upp till titelRow om man tar bort denna text. förslag på lösning? Lösning: lägg till float:left på signStepsBodyBorderBottom /johan  </span -->
  
</div>      
</div>  
<div id="documentBox">
  <script type="text/javascript">
    var docstate = $docstate:noescape()$;
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
  <span class="persondetails">
    
    <!-- 008 change/new -->
    <div class="signStepsBodyIcons">
      <span class="partnumber">PART 1</span>
      <a href="#plus2" class="plus"></a>
    </div>
    
    <!-- 013 changed -->
    <div class="sigentry signStepsBodyForms" id="peopleDetails">
      <input type="hidden" name="sigid" value="" />
      <span class="fields">
        <span class='dragfield sigfstname inputWrapper'>
          <span style="display:none;" class="fieldid">fstname</span>
          <span style="display:none;" class="type">author</span>
          <input type="text" infotext="Förnamn"
                 name="signatoryfstname" autocomplete="off" value="" />
          <img src="/theme/images/iconHand.png" class="draghandle"
               alt="drag" />
        </span>
        <span class='dragfield sigsndname inputWrapper'>
          <span style="display:none;" class="fieldid">sndname</span>
          <span style="display:none;" class="type">author</span>
          <input type="text" infotext="Efternamn"
                 name="signatorysndname" autocomplete="off" value="" />
          <img src="/theme/images/iconHand.png" class="draghandle"
               alt="drag" />
        </span>
        <span class='dragfield sigemail inputWrapper'>
          <span style="display:none;" class="fieldid">email</span>
          <span style="display:none;" class="type">author</span>
          <input type="email" infotext="E-post" name="signatoryemail" autocomplete="off" />
          <img src="/theme/images/iconHand.png" class="draghandle"
               alt="drag" />
        </span>
        <br/>
        <span class='dragfield sigcomp inputWrapper'>
          <span style="display:none;" class="fieldid">company</span>
          <span style="display:none;" class="type">sig</span>
          <input type="text" infotext="Företag" name="signatorycompany" autocomplete="off" />
          <img src="/theme/images/iconHand.png" class="draghandle"
               alt="drag" />
        </span>
        <span class='dragfield signum inputWrapper'>
          <span style="display:none;" class="fieldid">number</span>
          <span style="display:none;" class="type">sig</span>
          <input type="text" infotext="Org.nr./Pers.nr" name="signatorynumber" autocomplete="off" />
          <img src="/theme/images/iconHand.png" class="draghandle" 
               alt="drag" />
        </span>
      </span>
      <div class="otherfields">
      </div>
    </div>
  </span>
</div>
#####
validationdropdown= 
<div style="display:none">
  Verifieringsmetod<br>
  <select name="validationmethod"
          id="validationdropdown">
    <option value="email" selected>E-post</option>
    <option value="eleg">E-Legitimation</option>
  </select>
</div> 
#####
emailhidden=
<input type="hidden" value="email" name="validationmethod" />
