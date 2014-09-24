/**
 * Helper that helps with uploading sample documents.
 * Currently only base64 encoded PDF's.
 *
 * Example:
 * DocumentUploader.uploadByURL(
 *     function(documentData) { ... }, // success callback
 *     function(){},                   // error callback
 *     "/pdf/foo.base64.pdf",          // base64 pdf url
 *     "New title"                     // document title
 * );
 */

define(['Backbone', 'legacy_code'], function() {
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


    window.DocumentUploader = {
        /*
         * This function uploads the base64 encoded pdf that is supplied
         * as an url in the url argument and returns the document id of the new
         * document (by calling successCallback with the document json, parsed).
         * The document's title is supplied in documentTitle.
         *
         * Example usage:
         * uploadExampleDocument(function(documentData) { ... }, function(){}, "/pdf/foo.base64.pdf", "New title");
         */
        uploadByURL: function(successCallback, errorCallback, url, documentTitle) {
            var title = documentTitle || 'My first Scrive document';
            var xtoken = Cookies.get('xtoken');
            var boundary = '-------SCRIVESCRIVESCRIVE';

            $.ajax(url,
                   {cache: false,
                    success: function(data) {
                      var fakeUpload = getFakeFileUpload(data, title, xtoken, boundary);

                      $.ajax('/api/frontend/createfromfile/', {
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
                    }});
        }
    };

});
