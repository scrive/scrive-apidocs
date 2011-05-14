####		
docListTabs=
        <ul class="tabs">
          $docListTabItem(active=documentactive;link=linkdoclist;name="Dokument")$
          $docListTabItem(active=offeractive;link=linkofferlist;name="Offerter")$
          $docListTabItem(active=templateactive;link=linktemplatelist;name="Mallar")$
          <!-- comment out until functionality for using an existing attachment is added
                $docListTabItem(active=attachmentactive;link=linkattachmentlist;name="Bilagor")$ 
               just make it visible for those who knows to go to /a, so that we can still test the page-->
          $if(attachmentactive)$
            <li class="active"><a href="$link$" class="listtype">Bilagor</a></li>
          $endif$
        </ul>
####
docListTabItem=
          $if(active)$
            <li class="active"><a href="$link$" class="listtype">$name$</a></li>
          $else$
            <li><a href="$link$">$name$</a></li>
          $endif$
####
listShareDialog=
  <form method="post" name="form" action="$currentlink$" class="overlay redirectsubmitform" id="dialog-list-share-confirm" rel=".listForm" style="display:none;">
    <input type="hidden" name="share" value="true" />
    <div class="modal-container">
      <div class="modal-header">
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h2>Dela</h2>
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <p>Vill du dela detta med alla i din organisation.</p>
        </div>
      </div>
      <div class="modal-footer">        
        <a class="cancel close float-left" style="height:40px;">  
          <div class="label">Avbryt</div> 
        </a>
        <a class="btn-small green float-right submiter" href="#">
          <div class="left"></div>
          <div class="label"><p>OK</p></div>
          <div class="right"></div>
        </a>
      </div>
    </div>
  </form>
####
