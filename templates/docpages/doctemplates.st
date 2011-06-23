####
templatesForAjax= 

<div class="doctablecontainer">
<!-- Here the templates are generated -->
<!-- Table with all templates --> 
    <div class="tab-container">
            <div class="tab-content" style="background-color:#FFFFFF">
                <div class="float-left basicinfo"> 
                    <p>$_InfoSelectTemplate()$</p>
                    <a class="backicon float-left" href="/?doctype=$processid$"></a>      
                </div> 
                <div class="float-right search" style="margin-top: 30px;margin-right:30px;">
                   <form action="$currentlink$" method="POST">
                      $searchBox(infotext=_SearchTemplate()')$
                   </form>
                </div>
                <div class="clearfix"></div>
            </div>
            <div class="tab-table">
                <table>
                    <thead>
                        <tr>
                            <!--
                            <th style="width: 140px;">
                               $if(params.sorting.timeREV)$
                                <a href="$currentlink$&amp;sorting=time">Senaste händelse  <em> ▲</em></a>
                               $elseif(params.sorting.time)$
                                <a href="$currentlink$&amp;sorting=timeREV">Senaste händelse  <em style="text-color:red"> ▼</em></a>
                               $else$
                                <a href="$currentlink$&amp;sorting=time">Senaste händelse </a>
                               $endif$
                            </th>
                            <th style="width: 300px;">
                               $if(params.sorting.partnerREV)$
                                <a href="$currentlink$&amp;sorting=partner">Parter <em> ▲</em></a>
                               $elseif(params.sorting.partner)$
                                <a href="$currentlink$&amp;sorting=partnerREV">Parter  <em style="text-color:red"> ▼</em></a>
                               $else$
                                <a href="$currentlink$&amp;sorting=partner">Parter </a>
                               $endif$
                            </th>
                           -->
                            <th style="width: 400px;">
                               $if(params.sorting.titleREV)$
                                <a href="$currentlink$&amp;sorting=title">$_SortTemplate()$<em> ▲</em></a>
                               $elseif(params.sorting.title)$
                                <a href="$currentlink$&amp;sorting=titleREV">$_SortTemplate()$<em style="text-color:red"> ▼</em></a>
                               $else$
                                <a href="$currentlink$&amp;sorting=title">$_SortTemplate()$</a>
                               $endif$
                            </th>
                            <th></th>
                        </tr>
                    </thead>
                    
                    <tbody id="selectable">
                      $documents:{document |
                        <tr $if(document.odd)$class="odd"$endif$>
                            <!-- 
                            <td>
                                <span title="$document.mtime$">$document.mtime$</span>
                            </td>
                            <td> 
                                <form action="/t" method="POST">
                                <a class="submit">$document.signatories:{sig | $sig.name$}; separator=", "$</a>
                                <input type="hidden" name="template" value="$document.id$"/>
                                </form> 
                            </td>
                            -->
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
