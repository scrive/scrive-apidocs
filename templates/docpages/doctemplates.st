###
empty = $it.0$
####
templatesForAjax= 

<div class="doctablecontainer">
<!-- Create new template button (DISABLED FOR NOW)
  <form action="/d" method="post" enctype="multipart/form-data"  class="stepForm templateUploadForm" style="float:right;padding-top:10px;padding-right:10px"> 
        <input type="hidden" name="template" value="template">
        <div class="uploadButton" >  
          <a href="#attachFile"><span class="icon arrowRight"></span></a>
          <a href="#attachFile"> <span class="text">Skapa Ny Mall</span> </a> 
          <input class="multiFileInput submitOnUpload" maxlength="1" type="file" name="doc" accept="application/pdf" rel="templateUploadForm .filelist" />
        </div>

      <div class="filelist" style="padding-left:10px"></div>
  </form>
   <script> initFileInputs();</script>
-->
<!-- Here the templates are generated -->
<!-- Table with all templates --> <div class="tab-container">
            <div class="tab-content">
                <div class="col float-left">
                </div>
                <div class="col float-right">
                   <form action="$currentlink$" method="POST">
                      $searchBox(infotext='Sök dokument')$
                   </form>
                </div>
                <div class="clearfix"></div>
            </div>
            <div class="tab-table">
                <table>
                    <thead>
                        <tr>
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
                            <th style="width: 400px;">
                               $if(params.sorting.titleREV)$
                                <a href="$currentlink$&sorting=title">Dokument <em> ▲</em></a>
                               $elseif(params.sorting.title)$
                                <a href="$currentlink$&sorting=titleREV">Dokument <em style="text-color:red"> ▼</em></a>
                               $else$
                                <a href="$currentlink$&sorting=title">Dokument </a>
                               $endif$
                            </th>
                            <th></th>
                        </tr>
                    </thead>
                    
                    <tbody id="selectable">
                      $documents:{document |
                        <tr $if(document.odd)$class="odd"$endif$>
                            <td>
                                <span title="$document.mtime$">$document.mtime$</span>
                                $if(document.timeoutdaysleft)$  
                                  <span title="Förfallodatum: $document.timeoutdate$">
                                    <a href="#">
                                      ($document.timeoutdaysleft$)
                                    </a>
                                  </span> 
                                  $endif$
                            </td>
                            <td> 
                                <form action="/t" method="POST">
                                <a class="submit">$document.signatories:{sig | $sig.name$}; separator=", "$</a>
                                <input type="hidden" name="template" value="$document.id$"/>
                                </form> 
                            </td>
                            <td>
                                <form action="/t" method="POST">
                                <a class="submit">$document.documenttitle$</a>
                                <input type="hidden" name="template" value="$document.id$"/>
                                </form> 
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
####