####		
pageRubbishBinList=
$loadingdialog()$
<div> 
<form method="post" action="$currentlink$" enctype="multipart/form-data" class="listForm">
    <div id="tab-viewer">
        <div class="tab-header">
        $docListRubbishBinLink()$
        </div>
        $docListTabs()$
        <div class="tab-container">
            <div class="tab-content">
                <div class="col float-left">
                    <div class="tab-dd float-left">
                        <div class="tab-dd-button">
                          <a href="javascript:;"><p>$_options()$</p></a>
                        </div>
                        <ul class="tab-dd-opts">
                            <li><a href="javascript:;" rel="#dialog-list-restore-confirm" class="listAction">$_restore()$</a></li>
                            <li><a href="javascript:;" rel="#dialog-list-really-delete-confirm" class="listAction">$_delete()$</a></li>
                        </ul>
                    </div>
                </div>

                <div class="col float-right">
                      $searchBox(infotext=_SearchDoc())$
                </div>
                <div class="clearfix"></div>
            </div>
            <div class="tab-table">
                <table>
                    <thead>
                        <tr>
                            <th style="width: 30px;"><input type="checkbox" name="" id="all" /></th>
                            <th style="width: 140px;">
                               $if(params.sorting.timeREV)$
                                <a href="$currentlink$&amp;sorting=time">$_SortTime()$<em> ▲</em></a>
                               $elseif(params.sorting.time)$
                                <a href="$currentlink$&amp;sorting=timeREV">$_SortTime()$<em style="text-color:red"> ▼</em></a>
                               $else$
                                <a href="$currentlink$&amp;sorting=time">$_SortTime()$</a>
                               $endif$
                            </th>
                            <th style="width: 120px;">
                              $if(params.sorting.typeREV)$
                                <a href="$currentlink$&amp;sorting=type">$_SortType()$<em> ▲</em></a>
                               $elseif(params.sorting.type)$
                                <a href="$currentlink$&amp;sorting=typeREV">$_SortType()$<em style="text-color:red"> ▼</em></a>
                               $else$
                                <a href="$currentlink$&amp;sorting=type">$_SortType()$</a>
                               $endif$
                            </th>
                            <th style="width: 400px;">
                               $if(params.sorting.titleREV)$
                                <a href="$currentlink$&amp;sorting=title">$_SortDoc()$<em> ▲</em></a>
                               $elseif(params.sorting.title)$
                                <a href="$currentlink$&amp;sorting=titleREV">$_SortDoc()$<em style="text-color:red"> ▼</em></a>
                               $else$
                                <a href="$currentlink$&amp;sorting=title">$_SortDoc()$</a>
                               $endif$
                            </th>
                            <th></th>
                        </tr>
                    </thead>
                    
                    <tbody id="selectable">
                      $documents:{document |
                        <tr $if(document.odd)$class="odd"$endif$>
                            <td><input type="checkbox" name="doccheck" value="$document.documentid$" class="check"/></td>
                            <td>
                                <span title="$document.mtime$">$document.mtime$</span>
                            </td>
                            <td>
                                <span>$document.processname$ $if(document.istemplate)$ $_TemplateLabel()$ $endif$</span>
                            </td>
                            <td>
                                <a href="$document.doclink$" class="listname">$document.documenttitle$</a>
                                $if(document.davelink)$ 
                                <a href="$document.davelink$">*</a>     
                                $endif$
                            </td>
                            <td class="right">
                            </td>
                        </tr>
                      }$
                    </tbody>
                </table>
                $pagingFooter()$
            </div>
        </div>
    </div>
  </form>
  
  <form method="post" name="form" action="$currentlink$" class="overlay redirectsubmitform" id="dialog-list-restore-confirm" rel=".listForm" style="display:none;">
    <input type="hidden" name="restore" value="true" />
    <div class="modal-container">
      <div class="modal-header">
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h2>$_listRestoreDialogHead()$</h2>
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <p>$_listRestoreDialogBody()$</p>
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
  
  <form method="post" name="form" action="$currentlink$" class="overlay redirectsubmitform" id="dialog-list-really-delete-confirm" rel=".listForm" style="display:none;">
    <input type="hidden" name="reallydelete" value="true" />
    <div class="modal-container">
      <div class="modal-header">
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h2>$_listReallyDeleteDialogHead()$</h2>
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <p>$_listReallyDeleteDialogBody()$</p>
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
  
</div>
     
