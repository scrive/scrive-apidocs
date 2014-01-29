####
pageAttachment=
<div class="mainContainer" >
  <div class="body-container">
    <div class='attachment'></div>
        <script type="text/javascript">
         require(['jquery', 'legacy_code'], function(jQuery) {
          jQuery(".attachment").append(new KontraAttachment({id : "$id$", title: "$title$", editable : $if(editable)$ true $else$ false $endif$ }).view().el);
         });
        </script>
    </div>
</div>
####

