####		
pageAttachmentList=
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
                            <li><a href="javascript:;" rel="#dialog-list-share-confirm" class="listShare">$_pageAttachmentListShare()$</a></li>
                            <li><a href="javascript:;" rel="#dialog-list-delete-confirm" class="listDelete">$_delete()$</a></li>
                        </ul>
                    </div>

                    <a class="btn-tiny green float-left" style="overflow:hidden;height:26px;width:160px;margin-left: 20px;" >
                        <div class="left"> </div>
                        <div class="label">$_pageAttachmentListNewAttachment()$</div>
                        <div class="right"> </div>
                        <input class="multiFileInput submitOnUpload" maxlength="1" type="file" name="doc" accept="application/pdf" rel=".filelist" />
                    </a>
                    <div class="filelist" style="padding-left:10px;display:none"></div> 

                </div>

                <div class="col float-right">
                      $searchBox(infotext=_pageAttachmentListSearchInfo())$
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
                            <th style="width: 400px;">
                               $if(params.sorting.titleREV)$
                                <a href="$currentlink$&amp;sorting=title">$_SortAttachment()$<em> ▲</em></a>
                               $elseif(params.sorting.title)$
                                <a href="$currentlink$&amp;sorting=titleREV">$_SortAttachment()$<em style="text-color:red"> ▼</em></a>
                               $else$
                                <a href="$currentlink$&amp;sorting=title">$_SortAttachment()$</a>
                               $endif$
                            </th>
                            <th style="width: 100px;"></th>
                            <th></th>
                        </tr>
                    </thead>
                    
                    <tbody id="selectable">
                      $documents:{document |
                        <tr $if(document.odd)$class="odd"$endif$>
                            <td>$if(document.isauthor)$ <input type="checkbox" name="doccheck" value="$document.documentid$" class="check"/> $endif$</td>
                            <td>
                                <span title="$document.mtime$">$document.mtime$</span>
                            </td>
                            <td>
                                <a href="$document.doclink$" class="listname">$document.documenttitle$</a>
                                $if(document.davelink)$ 
                                <a href="$document.davelink$">*</a>     
                                $endif$
                            </td>
                            <td>
                                $if(document.isshared)$	$_pageAttachmentListIfShared()$$endif$
                            </td>
                            <td class="right">
                               <!--  <em>▼</em> -->
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
  $listLocalDialogs()$
  $listShareDialog()$
</div>
     
