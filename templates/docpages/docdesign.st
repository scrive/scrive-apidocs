#####
pageDocumentDesign=
<div style="display: none">
  <script type="text/javascript"> 
    var documentid = "$documentid$";
  </script>
  <script type="text/javascript" src="/js/bankid.js"></script>
</div>
<div id="signStepsWrapper">

  <div id="signStepsContainer" class="follow" >
    
    <div id="signStepsTitleRow">
      <span id="signStepsTitleRowTextContainer">
        $if(template)$
        <span class="title">$_signStepsTitleRowTextContainerIfTemplate()$</span>
        $else$
        <span class="title">$process.title$ #$documentid$:</span>
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
          $_step1selectIfTemplate()$
          $else$
          $process.step1text$
          $endif$
        </span>
      </div>
      
      <div id="step2select" class="$if(step2)$current$endif$">
        <a href="#" class="step2select clickArea"></a>
        <a href="#" class="step2select step2icon"> </a>
        <a href="#" class="step2select text">
          $if(template)$
          $_step2selectIfTemplate()$
          $elseif(isbasic)$
          $_step2selectIfisbasic()$
          $else$
          $_step2selectIfElse()$
          $endif$
        </a>  
      </div>
      $if(!isbasic)$
      <!-- Basic stuff doesn't have step 3 -->
      <div id="step3select" class="$if(step3)$current$endif$">
        <a href="#" class="step3select clickArea"></a>
        <a href="#" class="step3select step3icon"> </a>
        <a href="#" class="step3select text">
          $if(template)$
          $_step3selectIfTemplate()$
          $else$
          $_step3selectIfNotTemplate()$
          $endif$
        </a>
      </div>
      $endif$
      <div id="signStepsMenuEmptyStep">
        $if(!template)$
        $if(featureenabled)$
        <div class="signStepsBodySave extraLink">
          <small><a class="submit" form=".stepForm" rel="template">$_signStepsMenuEmptyStepSaveAsTemplate()$</a></small>
        </div>
        $endif$
        $endif$

        $if(isbasic)$
        $if(process.hasadvancedview)$
        <div class="signStepsFunctionalityChange  extraLink">
          <small><a id="toadvanced" class="switchIcon" rel="#dialog-confirm-advanced" style="padding-right:19px;">$_signStepsFunctionalityChangeToAdvanced()$</a></small>
        </div>
        $endif$
        $else$
        <div class="signStepsFunctionalityChange extraLink">
          <small><a id="tobasic" class="switchIcon" rel="#dialog-confirm-basic">$_signStepsFunctionalityChangeToSimplified()$</a></small>
        </div>
        $endif$
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
            $if(!isbasic)$
            <div id="signStepsBodyLeft">
              <div id="people">
                <div class="signStepsBodyTitle">
                  <span>$_signStep2ContentPersons()$</span><span id="signorderlisttitle" class="float-right">$_signStep2ContentSignOrder()$</span>
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
                  <span class="partnumber">$_personpanePart1()$</span>
                  $if(!isbasic)$
                  $if(!process.isauthorsend)$
                  <a href="#man2" class="man"></a>
                  $endif$
                  <a href="#" class="signingOrder"></a>
                  <select id="authorsignorder" name="authorsignorder" class="float-right">
                    <option value="1">1</option>
                    <option value=""></option>
                  </select>
                  $endif$
                  $if(featureenabled)$
                  <a class="plus" href="#"></a>
                  $endif$
                </div>
                
                <!-- 013 changed -->
                <div class="sigentry signStepsBodyForms $if(!featureenabled)$dragDisabled$endif$" id="peopleDetails">
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
                    <input type="radio" name="authorrole" id="authorsignatoryradio" value="signatory"><span class="radiolabel">$_authorsignatoryradioSenderSign()$</span></input>
                    <br/>
                    <input type="radio" name="authorrole" id="authorsecretaryradio" value="secretary"><span class="radiolabel">$_authorsignatoryradioSenderNotSign()$</span></input>
                  </div>                  
                </div>
              </span>
              
            </div>
            
            <div id="signStepsBodyFunctions">
              $if(!isbasic)$
              <div id="signStepsBodyFunctionsBox">
                <ul>
                  <li class="plus"><a class="designFunctionalLink" href="#add"  id="addSignatory">$_signStepsBodyFunctionsAddPerson()$</a></li>
                  <li class="minus"><a class="designFunctionalLink" href="#remove"  id="delSignatory">$_signStepsBodyFunctionsRemovePerson()$</a></li>
                </ul>
                <div class="signStepsButton" id="signStepsNextButton" >
                  <a href="#nextStep" id="nextstep" class="btn-small green center" style="display:inline-block">
                    <div class="left"></div>
                    <div class="label">
                        <p style="display:inline-block;width:80px;">$_signStepsBodyFunctionsNextStep()$</p>
                      <span class="btn-symbol green arrow-left"></span>
                    </div>
                    <div class="right"></div>
                  </a>  
                </div>
              </div>
              $else$
              <div id="signStepsBodyFunctionsBox" style="padding-left:17px;">
                $if(process.isvalidationchoiceforbasic)$
                <div>
                  <div style="padding-bottom:2px;padding-left:1px;">$_signStepsBodyFunctionsBoxChooseVerification()$</div>
                  <select name="validationmethod"
                          id="validationdropdown">
                    <option value="email" $if(emailselected)$selected="selected"$endif$>$_signStepsBodyFunctionsBoxEmail()$</option>
                    <option value="eleg" $if(elegselected)$selected="selected"$endif$>$_signStepsBodyFunctionsBoxEleg()$</option>
                  </select>
                </div>
                $endif$
                $if(process.isexpiryforbasic)$
                <div>
                  <input style="position:relative;top:2px;left:2px;margin-right:5px;" type="checkbox" class="addremovecheckbox flashOnOn" flashtype="red"
                         rel="#daystosignbox"
                         location="#datetosigncontainer"
                         oldlocation="#hiddenttimestuffbox"
                         autocomplete="off"
                         value="$if(timetosignset)$on$else$off$endif$"
                         id="daystosigncheckbox" />
                  <span class="hidden flashMessage">$process.expirywarntext$</span>
                   <label for="daystosigncheckbox">$_daystosigncheckboxExpDate()$</label>
                  <div id="datetosigncontainer">  </div>
                </div>
                $endif$
                <div class="signStepsBodyInvation" style="padding-top:4px;padding-bottom:9px;">
                   <small><a rel="#edit-invite-text-dialog" id="editinvitetextlink" href="#" style="padding-top:3px">$_editinvitetextlinkInviteText()$</a></small>
                  <input type="hidden" id="invitetext" name="invitetext" value="$documentinvitetext$" />
                </div>
                $if(template)$
                <div class="signStepButton" style="display:inline-block;padding-top:4px;">
                  <a class="btn-small green center submiter">
                    <div class="left"></div>
                    <div class="label">
                        <span>$_invitetextSaveTemplate()$</span>
                      <span class="btn-symbol green arrow-left"></span>
                    </div>
                    <div class="right"></div>
                  </a>
                </div>
                $elseif(process.isauthorsend)$
                <input type="hidden" value="email" name="validationmethod" />
                <div class="signStepButton" style="display:inline-block;padding-top:4px;">
                  <a type="button" id="sendinvite" rel="#dialog-confirm-sendinvite"  class="btn-small green center">
                    <div class="left"></div>
                    <div class="label" style="">
                      <span>$process.sendbuttontext$</span>
                      <span class="btn-symbol green arrow-left" ></span>
                    </div> 
                    <div class="right"></div>
                  </a>
                </div>
                $else$
                <div class="signbutton" style="display:inline-block;">
                  <a type="button" id="signinvite" rel="#dialog-confirm-signinvite" class="btn-small blue center">
                    <div class="left"></div>
                    <div class="label" style="width:134px;">
                       <span style="padding-left:9px;padding-right:9px">$_signinviteSkrivaPa()$</span>
                      <span class="btn-symbol cross"></span>
                    </div> 
                    <div class="right"></div>
                  </a>
                </div>
                $endif$
              </div>
              $endif$
            </div>
          </div>
        </div>  

        $if(!isbasic)$
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
                    <span class="hidden flashMessage">$process.expirywarntext$</span>
                    <label for="daystosigncheckbox">$_daystosigncheckboxActivateExpDate()$</label>
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
                <small><a rel="#edit-invite-text-dialog" id="editinvitetextlink" href="#" style="padding-top:3px">$_editinvitetextlink()$</a></small>
                <input type="hidden" id="invitetext" name="invitetext" value="$documentinvitetext$" />
              </div>
              <div class="signStepsBodyAttachments">
                <small>
                  <a rel="#update-attachments-dialog" href="#"
                     style="padding-top:3px" id="addattachmentlink">$_addattachmentlink()$</a>
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
                     style="padding-top:3px" id="addsigattachmentlink">$_addsigattachmentlink()$</a>
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
              <p>$_signStepsBodyFunctionssaveTemplate()$</p>
                  <span class="btn-symbol green arrow-left"></span>
                </div>
                <div class="right"></div>
              </a>
              $else$
              <div> 
                $validationdropdown()$
                $if(!process.isauthorsend)$
                <div style="padding-bottom:8px; padding-top: 2px;">
                  <input id="switchercheckbox" style="position:relative;top:2px" type="checkbox" class="sendcheckbox" on=".sendbutton" off=".signbutton" autocomplete="off" value="off" > 
                  <label for="switchercheckbox" class="usual" style="margin-left:3px;">$_switchercheckboxSignLast()$</label>
                  <label class="secretary"
                  style="margin-left:3px;display:none;">$_switchercheckboxNotSignatory()$</label></input>
                </div>
                $endif$
                <div class="sendbutton" style="$if(!process.isauthorsend)$display:none;$else$padding-top:20px;$endif$">
                  <a type="button" id="sendinvite" rel="#dialog-confirm-sendinvite"  class="btn-big green center">
                    <div class="left"></div>
                    <div class="label">
		                  <span>$process.sendbuttontext$</span>
                      <span class="btn-symbol green arrow-left"
                            style="position:relative;top:3px;right:0px"></span>
	                  </div> 
                    <div class="right"></div>
                  </a>
                </div>
                $if(!process.isauthorsend)$
                <div class="signbutton">
                  <a type="button"  id="signinvite" rel="#dialog-confirm-signinvite" class="btn-big blue center" style="float:none">
                    <div class="left"></div>
                    <div class="label" style="width:120px">
                    <span>$_signinviteSkrivaPa2()$</span>
                      <span class="icon cross"></span>
                    </div> 
                    <div class="right"></div>
                  </a>
                </div>
                $endif$
              </div>
              $endif$

            </div>
            
          </div>            
        </div>
        $endif$
      </form>
    </div>
  </div>
  $pageDocumentForAuthorHelpersLocalDialogs()$
  
</div>  
<div id="documentBox">
  <script type="text/javascript">
    var signingOrderEnabled = false;
    var allNonSignatories = false;
    var docstate = $docstate:noescape()$;
    var csvrowcount = $csvrowcount$;
    var initialperson = $initialperson$;
    var isaftercsvupload = $if(isaftercsvupload)$true$else$false$endif$;
    var issendonly = $if(process.isauthorsend)$true$else$false$endif$;
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
      $if(isbasic)$
      <input type="hidden" name="signatoryrole" value="signatory"/>
      $else$
      $if(process.isauthorsend)$
      <input type="hidden" name="signatoryrole" value="signatory"/>
      $else$
      <a href="#" class="man"></a>
      $endif$
      <a href="#" class="csv single" rel="#dialog-csvupload-1"></a>
      <a href="#" class="csv multi" rel="#dialog-csvupload-2" style="display:none;"></a>
      <a href="#" class="signingOrder"></a>
      <select class="signorder float-right" name="signatorysignorder"></select>
      $endif$
      $if(featureenabled)$
      <a href="#" class="plus"></a>
      $endif$
    </div>
    
    <!-- 013 changed -->
    <div class="sigentry signStepsBodyForms $if(!featureenabled)$dragDisabled$endif$" id="peopleDetails">

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
        <input type="hidden" name="signatoryrole" value="signatory"/>
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
    $if(featureenabled)$
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
dragndropIcon=
$if(featureenabled)$
<img src="/theme/images/iconHand.png" class="draghandle" alt="drag" />
$endif$
#####
validationdropdown= 
<div>
  <span>$_validationdropdownChooseVMethod()$</span>
  <select name="validationmethod"
          id="validationdropdown">
    <option value="email" $if(emailselected)$selected="selected"$endif$>$_validationdropdownIfEmail()$</option>
    <option value="eleg" $if(elegselected)$selected="selected"$endif$>$_validationdropdownIfEleg()$</option>
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
          <h2>$_pageDocumentForAuthorHelpersLocalDialogsHead()$</h2>
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <div>     
            <a href="#" class="plus" >  </a>$_pageDocumentForAuthorHelpersLocalDialogsAddAttachment()$
          </div>
          <table id="sigattachmenttable">
            <thead>
              <tr>
                <th class="minusbutton">   $_pageDocumentForAuthorHelpersLocalDialogsMinusbutton()$</th>
                <th class="attachmentname">$_pageDocumentForAuthorHelpersLocalDialogsAttachmentname()$</th>
                <th class="attachmentdesc">$_pageDocumentForAuthorHelpersLocalDialogsAttachmentdesc()$</th>
                <th class="attachmentsigs">$_pageDocumentForAuthorHelpersLocalDialogsAttachmentsigs()$</th>
                <th class="attachmentsels">$_pageDocumentForAuthorHelpersLocalDialogsAttachmentsels()$</th>
              </tr>
            </thead>
            <tbody>
              $sigattachments:{a|
              <tr>
                <td><a href="#" class="minus"></a></td>
                <td><input type="text" name="sigattachname"
                           value="$a.attachmentname$" /></td>
                <td><textarea name="sigattachdesc" value="$a.attachmentdescription$" >$a.attachmentdescription$</textarea></td>
                <td>
                  <select class="signatoryselector">
                    <option selected="selected">$_pageDocumentForAuthorHelpersLocalDialogsRecipient()$</option>
                  </select>
                </td>
                <td>
                  <span class="selectedsigspan">
                    <ul class="selectedsigs">
                      $a.signatories:{s|
                      <li>
                        $s.signame$ 
                        <a href="#" class="remove">x</a>
                        <span style="display:none" class="sigemail">$s.email$</span>
                      </li>
                      }$
                    </ul>
                    <input type="hidden" name="sigattachemails"
                           value="$a.sigemails$" />
                  </span>
                </td>
              </tr>
              }$
            </tbody>
          </table>
        </div>
      </div>
      <div class="modal-footer">
        <div class="float-left">
          <a class="close cancel">$_pageDocumentForAuthorHelpersLocalDialogsCancel()$</a>
        </div>
        <a class="btn-small green float-right submiter">
          <div class="left"></div>
          <div class="label">
            <p>
              $_pageDocumentForAuthorHelpersLocalDialogsSave()$
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
          <h2>$_update-attachments-dialogHead()$</h2>
        </span>
        <a class="modal-close close"> </a>
      </div>

      <div class="modal-body">

        <div class="modal-content">
          <a href=#" id="attachmentback" style="display:none;"> </a>
          <div>     
           $_update-attachments-dialogChooseAttachment()$
          </div>
          <span id="attachmentbuttons">
            <div class="signStepsBodyUploadBox">
              <div>
                <span id="signStepsBodyUploadTextContainer"> 
                  <span class="header">$_update-attachments-dialogUpLoad()$</span><br/>
                  <span class="text">$_update-attachments-dialogUpLoadOnlyPDF()$</span> 
                </span>
              </div>
              <br/>
              <div class="signStepsButtonContainer">
                <a class="btn-small green">  
                  <div class="left"> </div>
                  <div class="label" style="width:80px">$_update-attachments-dialogUpLoadChooseFile()$</div> 
                  <div class="right"> </div>
                  <input class="multiFileInput" style="top:-40px;right:0px" maxlength="20" type="file" name="attachment" accept="application/pdf" rel="#tobeattached" />
                </a>
              </div>
            </div>
            <div class="signStepsBodyUploadBox">
              <div>
                <span id="signStepsBodyUploadTextContainer"> 
                  <span class="header">$_update-attachments-dialogUpLoadChooseAttachment()$</span><br/>
                  <span class="text">$_update-attachments-dialogUpLoadSaved()$</span>
                </span>
              </div>
              <br/>
              <div class="signStepsButtonContainer">
                <a id="showexistingattachments" class="btn-small green">  
                  <div class="left"> </div>
                  <div class="label" style="width:80px">$_update-attachments-dialogUpLoadChooseExistingAttachment()$</div>
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
                <a href="#" class="removesig">x</a><span style="padding-left:5px;">$attachment.attachmentname$</span>
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
          <a class="close cancel">$_update-attachments-dialogUpLoadFooterCancel()$</a>
        </div>
        <a class="btn-small green float-right submitAttachments">
          <div class="left"></div>
          <div class="label">
            <p>
              $_update-attachments-dialogUpLoadFooterAttach()$
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
          <h2>$_dialog-csvupload-1Head()$</h2>
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <div>$_dialog-csvupload-1Body()$</div>
          <div class="signStepsBodyUploadBox">
            <div>
              <span id="signStepsBodyUploadTextContainer"> 
                <span class="header">$_dialog-csvupload-1ChooseFileToUpLoad()$</span><br/>
                <span class="text">$_dialog-csvupload-1CSVOnly()$</span>
              </span>
            </div>
            <div class="signStepsButtonContainer">
              <a class="btn-small green">  
                <div class="left"> </div>
                <div class="label" style="width:80px">$_dialog-csvupload-1ChooseFile()$</div>
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
          <h2>$_dialog-csvupload-2Head()$</h2>
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
                        <span>$_dialog-csvupload-2FooterBack()$</span>
                  </div>
                  <div class="right"></div>
                </a>
                $endif$
                $if(!csvproblem.islastproblem)$
                <a class="btn-tiny black float-right nextproblem" href="#">
                  <div class="left"></div>
                  <div class="label">
                        <span>$_dialog-csvupload-2FooterNext()$</span>
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
                        <th>$_dialog-csvupload-2CSVDataFirstName()$</th>
                        <th>$_dialog-csvupload-2CSVDataLastName()$</th>
                        <th>$_dialog-csvupload-2CSVDataEmail()$</th>
                        <th>$_dialog-csvupload-2CSVDataCompany()$</th>
                        <th>$_dialog-csvupload-2CSVDataPersonalNumber()$</th>
                        <th>$_dialog-csvupload-2CSVDataOrgNumber()$</th>
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
                        <span>$_dialog-csvupload-2CSVProblemBack()$</span>
    </div>
    <div class="right"></div>
  </a>
  $endif$
  $if(!csvpage.islastcsvpage)$
  <a class="btn-tiny black float-right nextcsvpage" href="#">
    <div class="left"></div>
    <div class="label">
                        <span>$_dialog-csvupload-2CSVProblemNext()$</span>
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
          <div class="label">$_dialog-csvupload-2CSVMultiChooseFile()$</div>
    <input class="multiFileInput submitOnUpload" style="top:-40px;right:0px;width:50px;" maxlength="1" type="file" name="csv" accept="application/csv" rel=".filelist"/>
    <div class="filelist" style="display:none;"></div>
  </a>
  
  <a class="btn-small green float-right close" href="#">
    <div class="left"></div>
          <div class="label"><p>$_dialog-csvupload-2CSVMultiSave()$</p></div>
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
          <h2 id="dialog-title">$_dialog-confirm-advancedChangeToHead()$</h2>
      </span>
      <a class="modal-close close"> </a>
    </div>
    <div class="modal-body">
      <div class="modal-content">
          <p>$_dialog-confirm-advancedBody()$</p>
      </div>
    </div>
    <div class="modal-footer">
        <a  class="close cancel float-left">$_dialog-confirm-advancedChangeToFooterCancel()$</a>
      <a href="#" class="btn-small green float-right submiter toadvanced">
        <div class="left"></div>
        <div class="label">
            <span>$_dialog-confirm-advancedChangeToFooterChangeProcess()$</span>
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
          <h2 id="dialog-title">$_dialog-confirm-basicChangeToHead()$</h2>
      </span>
      <a class="modal-close close"> </a>
    </div>
    <div class="modal-body">
      <div class="modal-content">
        <p>
            $_dialog-confirm-basicChangeToBody()$</p>
        <p> 
            $_dialog-confirm-basicChangeToBody2()$</p>
      </div>
    </div>
    <div class="modal-footer">
        <a  class="close cancel float-left">$_dialog-confirm-basicChangeToFooterCancel()$</a>
      <a href="#" class="btn-small green float-right submiter tobasic">
        <div class="left"></div>
        <div class="label">
            <span>$_dialog-confirm-basicChangeToChangeProcess()$</span>
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
          <h2 id="dialog-title-sign">$_dialog-confirm-signinviteHead()$</h2>
      </span>
      <a class="modal-close close"> </a>
    </div>
    <div class="modal-body">
      <div class="modal-content">
          <p>$_dialog-confirm-signinviteBody()$<strong class="visible-docname">$documenttitle$</strong>?</p>
          <p>$_dialog-confirm-signinviteBody2()$<span class="Xinvited">Invited</span>$_dialog-confirm-signinviteBody3()$</p>
        <span class="eleghidden" style="display:none;">
            <h3>$_dialog-confirm-signinviteSubHead()$</h3>
            <p>$_dialog-confirm-signinviteSubBody()$<a href="http://www.e-legitimation.se/Elegitimation/Templates/LogolistPageTypeB.aspx?id=86"
                        target="_new">$_dialog-confirm-signinviteSubBody2ClickHere()$</a>$_dialog-confirm-signinviteSubBody3()$</p>
        </span>
      </div>
    </div>
    <div class="modal-footer">
      <input type="hidden" name="sign" value="automatic" id="signauto" />
        <a class="close cancel float-left">$_dialog-confirm-signinviteFooterCancel()$</a>
      <a href="#" class="btn-small blue float-right submiter signhidden">
        <div class="left"></div>
        <div class="label">
            <p>$_dialog-confirm-signinviteFooterSkrivaPa()$</p>
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
          $process.confirmsendtitle$
        </h2>
      </span>
      <a class="modal-close close"> </a>
    </div>
    <div class="modal-body">
      <div class="modal-content">
        $if(!process.isauthorsend)$ 
        <div id="dialog-confirm-text-send">
            <p>$_dialog-confirm-sendinviteBody()$<strong class="visible-docname">$documenttitle$</strong>$_dialog-confirm-sendinviteBody2To()$<span class="Xinvited">Invited</span>?</p>
        </div>
        <div id="dialog-confirm-text-send-fields">
            <p>$_dialog-confirm-sendinviteBody3()$</p>
            <p>$_dialog-confirm-sendinviteBody4()$<strong class="visible-docname">$documenttitle$</strong>$_dialog-confirm-sendinviteBody5()$<span class="Xinvited">Invited</span>?  </p>
        </div>

        <div id="dialog-confirm-text-send-normal">
            <p>$_dialog-confirm-text-send-normalBody1()$<span class="Xinvited">Invited</span>$_dialog-confirm-text-send-normalBody2()$</p>
        </div>
        $else$
        <div id="dialog-confirm-text-send">
              <p>$process.confirmsendtext$ <strong class="visible-docname">$documenttitle$</strong>$_dialog-confirm-text-sendIfNotNormalOrFieldsTo()$<span class="Xinvited">Invited</span>?</p>
        </div>
        $endif$
      </div>
    </div>
    <div class="modal-footer">
      <input type="hidden" name="final" value="automatic" />
        <a  class="close cancel float-left">$_dialog-confirm-text-sendFooterCancel()$</a>
      <a href="#" class="btn-small green float-right submiter">
        <div class="left"></div>
        <div class="label">
            <span>$_dialog-confirm-text-sendFooterSend()$</span>
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
          <h2>$_edit-invite-text-dialogHead()$</h2>
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
        <a class="close cancel">$_edit-invite-text-dialogFooterCancel()$</a>
        <a class="editer message">$_edit-invite-text-dialogFooterOwnMessage()$</a>
      </div>
      <a href="#" class="btn-small green float-right close" id="editing-invite-text-finished">
        <div class="left"></div>
        <div class="label">
            <p>$_edit-invite-text-dialogFooterOk()$</p>
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
        $process.expirytext$ <input class="signStepsBodyDaysLeftInput" type="text" id="daystosign" name="daystosign" value="$documentdaystosignboxvalue$" maxlength="2" size="2" autocomplete="off" />$_signStepsBodyDaysLeftToSign()$</span>
    <div  class="datetodaystip signStepsBodyCalendar" rel="#daystosign"> </div>
  </div>
</div>
</div>
#####
###
contracttitle=$_contracttitle()$
###
offertitle=$_offertitle()$
###
ordertitle=$_ordertitle()$
###
contractname=$_contractname()$
###
offername=$_offername()$
###
ordername=$_ordername()$
###
contractstep1text=$_contractstep1text()$
####
offerstep1text=$_offerstep1text()$
###
orderstep1text=$_orderstep1text()$
###
contractexpirywarntext=$_contractexpirywarntext()$
####
offerexpirywarntext=$_offerexpirywarntext()$
####
orderexpirywarntext=$_orderexpirywarntext()$
####
contractsendtext=$_contractsendtext()$
####
offersendtext=$_offersendtext()$
####
ordersendtext=$_ordersendtext()$
####
contractconfirmsendtitle=$_contractconfirmsendtitle()$
####
offerconfirmsendtitle=$_offerconfirmsendtitle()$
####
orderconfirmsendtitle=$_orderconfirmsendtitle()$
####
contractconfirmsendtext=$_contractconfirmsendtext()$
####
offerconfirmsendtext=$_offerconfirmsendtext()$
####
orderconfirmsendtext=$_orderconfirmsendtext()$
####
contractexpirytext=$_contractexpirytext()$
####
offerexpirytext=$_offerexpirytext()$
####
orderexpirytext=$_orderexpirytext()$
####
