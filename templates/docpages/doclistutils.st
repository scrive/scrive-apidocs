####		
docListTabs=
<ul class="tabs">
  $docListTabItem(active=documentactive;link=linkdoclist;name=_docListTabsDocs())$
  $docListTabItem(active=offeractive;link=linkofferlist;name=_docListTabsOffers())$
  $docListTabItem(active=orderactive;link=linkorderlist;name=_docListTabsOrders())$
  $docListTabItem(active=templateactive;link=linktemplatelist;name=_docListTabsTemplates())$
  $docListTabItem(active=attachmentactive;link=linkattachmentlist;name=_docListTabsAttachemtns())$ 
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
          <h2>$_listShareDialogHead()$</h2>
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <p>$_listShareDialogBody()$</p>
        </div>
      </div>
      <div class="modal-footer">        
        <a class="cancel close float-left" style="height:40px;">  
          <div class="label">$_cancel()$</div>
        </a>
        <a class="btn-small green float-right submiter" href="#">
          <div class="left"></div>
          <div class="label"><p>$_ok()$</p></div>
          <div class="right"></div>
        </a>
      </div>
    </div>
  </form>
####
