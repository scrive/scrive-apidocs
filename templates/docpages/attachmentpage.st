####
pageAttachment=
<div class='attachment'></div>
<script type="text/javascript">
    jQuery(".attachment").append(new KontraAttachment({id : "$id$", title: "$title$", editable : $if(editable)$ true $else$ false $endif$ }).view().el);
</script>

