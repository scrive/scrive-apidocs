(function() {

    window.FileUpload = {
        upload: function(args) {
            args = args || {};
            var body = $('body');

            var form = $('<form />');
            form.attr('method', 'POST');
            form.attr('enctype', 'multipart/form-data');
            form.attr('action', args.action || '');
            form.hide();

            var input = $('<input />');
            input.attr('type', 'file');
            input.attr('name', args.name);
            input.attr('accept', args.mimetype);

            var xtoken = $('<input />');
            xtoken.attr('type', 'hidden');
            xtoken.attr('name', 'xtoken');
            xtoken.attr('value', readCookie("xtoken") || '');

            form.append(input);
            form.append(xtoken);

            body.append(form);

            input.change(function() {
                if(args.beforeUpload)
                    args.beforeUpload();
                form.ajaxForm({dataType: args.dataType,
                               success: args.success,
                               error: args.error
                              });
                form.submit();
            });

            input.click();
        }
    };

}());
