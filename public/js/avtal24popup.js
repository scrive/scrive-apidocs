/* Modal to direct users to Avtal24.
 * Usage: new Avtal24Popup()
*/

$(function(){

window.Avtal24Popup = function() {
      var content = $("<div/>").append($("<div style='font-size: 14px; font-weight: bold; color: #000000; text-align: center; margin:10px auto'/>").text(localization.avtal24.description))
      content.append('<iframe width="634" height="476" src="https://www.youtube-nocookie.com/embed/Hkm7xzrhYic" frameborder="0" allowfullscreen></iframe>')
      Confirmation.popup({
              content  : content,
              title  : localization.avtal24.title,
              acceptText: localization.avtal24.go,
              width: "800px",
              onAccept : function() {
                  window.location = "https://avtal24.se/scrive";
                  return true;
              }    
            });
};

});
