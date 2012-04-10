####
docListRubbishBinLink=
  <span class="float-right" style="margin-right:5px"><a href="$linkrubbishbinlist$" class="rubbishbin $if(rubbishbinactive)$highlight$endif$"></a></span>
####		
docListTabs=
<ul class="tabs">
  $if(rubbishbinactive)$
  $docListTabItem(active=rubbishbinactive;link=linkrubbishbinlist;name=_docListTabsRubbishBin())$
  $else$
  $docListTabItem(active=documentactive;link=linkdoclist;name=_docListTabsDocs())$
  $docListTabItem(active=offeractive;link=linkofferlist;name=_docListTabsOffers())$
  $docListTabItem(active=orderactive;link=linkorderlist;name=_docListTabsOrders())$
  $docListTabItem(active=templateactive;link=linktemplatelist;name=_docListTabsTemplates())$
  $docListTabItem(active=attachmentactive;link=linkattachmentlist;name=_docListTabsAttachemtns())$
  <!-- $docListTabItem(active=paddevicearchiveactive;link=linkpaddevicearchive;name=_PadDevice())$ -->
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
docListAttachmentLabel=$_docListAttachmentLabel()$
####
docListTemplateLabel=$processname$ ($_docListTemplateLabel()$)
####
