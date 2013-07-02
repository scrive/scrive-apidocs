(function(window) {
    window.FakeFileUpload = {};

    /*
     * Fake a form submission that has a <input type="file"> in it. 
     * 
     * We can't set the filename in any other way (i.e. there's no way to use 
     * Submit because only when there's a type=file the filename attribute is 
     * attached to the submission).  
     */
    var getFakeFileUpload = function(filedata, filename, xtoken, boundary) {
        var content = [];

        content.push('--' + boundary);
        content.push('Content-Disposition: form-data; name="xtoken"');
        content.push('');
        content.push(xtoken);

        content.push('--' + boundary);
        content.push('Content-Disposition: form-data; name="file"; filename="' + filename + '"');
        content.push('Content-Type: application/pdf');
        content.push('');
        content.push(filedata);
        content.push('--' + boundary + "--");

        return content.join('\r\n');
    };

    /*
     * This function uploads the base64 encoded pdf that is supplied 
     * as an url in the url argument and changes the main file of document with id
     * documentId to it.
     * The document's title is supplied in documentTitle.
     */
    var changeMainFile = function(successCallback, errorCallback, url, documentId, documentTitle) {
        var title = documentTitle || 'My first Scrive document';
        var xtoken = Cookies.get('xtoken');
        var boundary = '-------SCRIVESCRIVESCRIVE';

        $.get(url, function(data) {
            var fakeUpload = getFakeFileUpload(data, title, xtoken, boundary);
            
            $.ajax('/api/frontend/changemainfile/' + documentId, { 
                'contentType': "multipart/form-data; boundary="+boundary, 
                data: fakeUpload, 
                'type': 'POST',
                'dataType': 'json',
                success: function(data) {
                    successCallback(data);
                },
                error: function() {
                    errorCallback();
                }
            });
        });
    };

    window.FakeFileUpload.changeMainFile = changeMainFile;

})(window);
