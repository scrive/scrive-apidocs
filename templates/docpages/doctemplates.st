####
templatesForAjax= 

<div class="doctablecontainer">
<!-- Create new template button -->
  <form action="/d" method="post" enctype="multipart/form-data"  class="stepForm templateUploadForm" style="float:right;padding-top:10px;padding-right:10px"> 
        <input type="hidden" name="template" value="template">
        <div class="uploadButton" >  
          <a href="#attachFile"><span class="icon arrowRight"></span></a>
          <a href="#attachFile"> <span class="text">Skapa Ny Mall</span> </a> 
          <input class="multiFileInput submitOnUpload" maxlength="1" type="file" name="doc" accept="application/pdf" rel="templateUploadForm .filelist" />
        </div>

      <div class="filelist" style="padding-left:10px"></div>
  </form>
  <script> initFileInputs() ;</script>
<!-- Table with all templates -->
  <table class="doctable" cellspacing="0">
    <col>
    <col>
    <col>
    <col>
    <thead>
      <tr>
        <td><a href="#" id="all">Alla</a></td>
        <td><span style="padding-left:5px"></span></td> 
        <td></td>
        <td>Mall</td>
      </tr>
    </thead>
    <tfoot>
      <tr>
        <td colspan="7" style="text-align: right; overflow: hidden;">
        </td>
      </tr>
    </tfoot>
    <tbody id="selectable">
      $templates:{template |
      
      <tr class="ui-state-default">
        <td class="tdleft">
           <input type="checkbox" name="doccheck" value="$template.id$" class="check">
        </td>
        <td>
        </td>
        <td>
         
        </td>
        <td><a href="$template.link$">$template.name$</a></td>
        <td class="tdright">
        </td>
      </tr>
      
      }$
    </tbody>
  </table>  
  </div>
####