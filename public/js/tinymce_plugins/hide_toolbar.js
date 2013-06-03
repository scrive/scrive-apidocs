tinymce.PluginManager.add('hide_toolbar', function(editor, url) {
  editor.on('PreInit', function() {
    $(editor.getContainer()).find('div[role=toolbar]').hide()
  });
});
