####		
pageTemplatesList=
<div> 
<form method="post" action="$currentlink$" enctype="multipart/form-data" class="listForm">
    <div id="tab-viewer">
        <div class="tab-header">

        </div>
        <ul class="tabs">
            <li><a href="/d">Dokument</a></li>
            <li class="active"><a href="/t" class="listtype">Mallar</a></li>
            <!-- <li><a href="/contacts">Kontakter</a></li> -->
        </ul>
        <div class="tab-container">
            <div class="tab-content">
                <div class="col float-left">
                    <div class="tab-dd">
                        <a href="javascript:;"><p>Alternativ</p></a>
                        <ul class="tab-dd-opts">
                            <li><a href="javascript:;" rel="#dialog-list-delete-confirm" class="listDelete">Radera</a></li>
                        </ul>
                    </div>
                </div>

                <div class="col float-right">
                      $searchBox(infotext='Sök dokument')$
                </div>

                <div class="col" style="margin-left:44%;margin-right:44%;padding-top:15px;padding-bottom:0px">
                    <a class="btn-small green" style="overflow:hidden;height:34px;width:114px" >  
                        <div class="left"> </div>
                        <div class="label"> Skapa ny mall </div> 
                        <div class="right"> </div>
                        <input class="multiFileInput submitOnUpload" style="top:-40px;right:0px" maxlength="1" type="file" name="doc" accept="application/pdf" rel=".filelist" />
                    </a>
                <div class="filelist" style="padding-left:10px;display:none"></div> 
                </div>
                <div class="clearfix"></div>
            </div>
            <div class="tab-table">
                <table>
                    <thead>
                        <tr>
                            <th style="width: 30px;"><input type="checkbox" name="" id="all" /></th>
                            <!-- 
                            <th style="width: 140px;">
                               $if(params.sorting.timeREV)$
                                <a href="$currentlink$&sorting=time">Senaste händelse  <em> ▲</em></a>
                               $elseif(params.sorting.time)$
                                <a href="$currentlink$&sorting=timeREV">Senaste händelse  <em style="text-color:red"> ▼</em></a>
                               $else$
                                <a href="$currentlink$&sorting=time">Senaste händelse </a>
                               $endif$
                            </th>
                            <th style="width: 300px;">
                               $if(params.sorting.partnerREV)$
                                <a href="$currentlink$&sorting=partner">Parter <em> ▲</em></a>
                               $elseif(params.sorting.partner)$
                                <a href="$currentlink$&sorting=partnerREV">Parter  <em style="text-color:red"> ▼</em></a>
                               $else$
                                <a href="$currentlink$&sorting=partner">Parter </a>
                               $endif$
                            </th>
                            -->
                            <th style="width: 600px;">
                               $if(params.sorting.titleREV)$
                                <a href="$currentlink$&sorting=title">Mall <em> ▲</em></a>
                               $elseif(params.sorting.title)$
                                <a href="$currentlink$&sorting=titleREV">Mall <em style="text-color:red"> ▼</em></a>
                               $else$
                                <a href="$currentlink$&sorting=title">Mall </a>
                               $endif$
                            </th>
                            <th></th>
                        </tr>
                    </thead>
                    
                    <tbody id="selectable">
                      $documents:{document |
                        <tr $if(document.odd)$class="odd"$endif$>
                            <td>$if(document.isauthor)$ <input type="checkbox" name="doccheck" value="$document.documentid$" class="check"> $endif$</td>
                            <!-- <td>
                                <span title="$document.mtime$">$document.mtime$</span>
                            </td>
                            <td><a href="$document.doclink$">$document.signatories:{sig | $sig.name$}; separator=", "$</a></td>
                            -->  
                            <td>
                                <a href="$document.doclink$" class="listname">$document.documenttitle$</a>
                                $if(document.davelink)$ 
                                <a href="$document.davelink$">*</a>     
                                $endif$
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
</div>
     