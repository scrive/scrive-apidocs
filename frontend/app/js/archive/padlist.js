/* Definition of rubish bin archive */

define(['Backbone', 'legacy_code'], function() {


var SelectPartyModal = function(signingIndexes,listobject) {
          var self = this;
          self.current = 0;
          var modalContent = function() {
            var div = $("<div style='height:32px;'/>");
            var label = $("<label style='float:left;padding-right:10px;line-height: 32px;'>").text(localization.pad.giveForSigningThisDevice + " " );
            div.append(label);
            var options = [];
            for(var i=0;i<signingIndexes.length;i++) {
                var currentName = listobject.subfield(signingIndexes[i],"name").trim();
                if (i != self.current)
                options.push({
                  name: (currentName != "" ? currentName : localization.process.signatoryname + " " + signingIndexes[i]),
                  value : i
                });
            };
            var currentName = listobject.subfield(signingIndexes[self.current],"name").trim();
            var select = new Select({
                name : ( currentName != "" ? currentName : localization.process.signatoryname + " " + signingIndexes[self.current]),
                cssClass : "float-left",
                options : options,
                onSelect : function(v) {
                  self.current = v;
                  self.onChange();
                  return true;
                }
            });
            return div.append(select.el());
          };
          self.content = modalContent();
          self.onChange = function() {
            var c = modalContent();
            self.content.replaceWith(c);
            self.content = c;
          };
          new Confirmation({
            title : localization.authorview.goToSignView,
            content :self.content,
            onAccept : function() {
                mixpanel.track('Give for pad signing to some pad signatory - opening signview -from list');
                new Submit({
                   url : "/padsign/" + listobject.field("id") + "/" + listobject.subfield(signingIndexes[self.current],"id"),
                   method : "POST"
                }).send();
            }
          });


}

window.PadListRefresher = function(padlist) {
  var currentCounter = 1;
  var currentInterval = 1;
  return {
    resetCounter : function() {
      currentCounter = 1;
      currentInterval = 1;
    },
    step : function() {
      currentCounter--;
      if (currentCounter <= 0) {
            padlist.fetchWithCallback(function(currentlist,newlist) {
            if (currentlist.length != newlist.length) {
              padlist.recall({silent : true});
              currentCounter = 1;
              currentInterval = 1;
            }
        });
        currentInterval = Math.min(currentInterval + 1,30);
        currentCounter = currentInterval;
      }
    }
  }
}

window.PadList = function() {
    var refresher;// This will be initated later due to dependency to list;
    var list = new KontraList({
      name : "Sign now list",
      schema: new Schema({
      url: "/api/frontend/list",
      extraParams : { documentType : "DocumentsForPad"},
      sorting: new Sorting({ fields: ["title", "time"]}),
      paging: new Paging({showOnlyForMultiplePages : true}),
      minRows: 7,
      cells :  [
        new Cell({name: "ID", width:"30px", field:"id", special: "select"}),
        new Cell({name: localization.archive.documents.columns.status, width:"62px", field:"status",
                 rendering: function(status,idx,listobject) {
                    var icon = jQuery("<div class='icon status "+status+"'></div>");
                    var tip = jQuery("<div id='tooltip-"+status+"'> <div class='icon status "+status+"'></div><p>"+
                                      localization.statusToolTip[status]+"</p></div>");
                    ToolTip.set({
                        on: icon,
                        tip: tip
                    });
                    return icon;
                 }
        }),
        new Cell({name: localization.archive.documents.columns.time, width:"105px", field:"time", special: "rendered",
                  rendering: function(time) {
                         if (time != undefined && time != "")
                           return $("<span/>").text(new Date(Date.parse(time)).toTimeAbrev()).attr("title",new Date(Date.parse(time)).fullTime());
                         else return $("<span/>");
        }}),
        new Cell({name: localization.archive.documents.columns.party, width:"210px", field:"party", special: "expandable", subfield : "name"}),
        new Cell({name: localization.archive.documents.columns.title, width:"230px", substyle: "", field:"id",special: "rendered",
                 rendering: function(value,idx,listobject) {
                    if (idx == undefined)
                       {

                            var link =  $("<a class='s-archive-document-title' href='#'/>").text(listobject.field("title"));
                            link.click(function() {
                              var signingIndexes = [];
                              LocalStorage.set("pad","from-list","true");
                              for(var i=0; i< listobject.subfieldsSize(); i++)
                                  if (listobject.subfield(i,"cansignnow") == true && listobject.subfield(i,"delivery") =="pad")
                                    signingIndexes.push(i);
                              if (signingIndexes.length > 1) {
                                    new SelectPartyModal(signingIndexes,listobject)
                              }
                              else
                                new Submit({
                                  url : "/padsign/" + listobject.field("id") + "/" + listobject.subfield(signingIndexes[0],"id"),
                                  method : "POST"
                                }).send();
                              return false;
                            });
                            return link;
                        }
                 }})
       ],
       actions : [
         new ListAction({
           name : "Update",
           cssClass : "float-right",
           acceptEmpty : true,
           onSelect : function() {
             list.recall();
             if (refresher != undefined)
               refresher.resetCounter();
          }
         }),
         new ListAction({
           name : localization.archive.documents.remove.action,
           emptyMessage :  localization.archive.documents.cancel.emptyMessage,
           size: 'normal',
           avaible : function(doc){ return true;},
           onSelect : function(docs) {
             var confirmtext = jQuery("<p/>").append(localization.archive.documents.remove.body + " ");
             var label = jQuery("<strong/>");
             if (docs.length == 1) {
               confirmtext.append(jQuery("<strong/>").text(docs[0].field("title")));
             } else {
               confirmtext.append(docs.length + (" " + localization.documents).toLowerCase());
             }
             confirmtext.append("?");
             var confirmationPopup = new Confirmation({
               acceptText: localization.ok,
               rejectText: localization.cancel,
               title: localization.archive.documents.remove.action,
               icon: '/img/modal-icons/delete.png',
               content: confirmtext,
               onAccept : function() {
                 mixpanel.track('Delete document');
                 new Submit({
                   url: "/d/delete",
                   method: "POST",
                   documentids: "[" + _.map(docs, function(doc){return doc.field("id");}) + "]",
                   ajaxsuccess : function() {
                     new FlashMessage({color : "green", content : localization.archive.documents.remove.successMessage});
                     list.recall();
                     confirmationPopup.clear();
                   }
                }).sendAjax();
              }
            });
            return true;
           }
         })
        ]

      })

    });

    refresher = new PadListRefresher(list);
    var refresherStep = function() {
      setTimeout(function() {
        refresher.step();
        refresherStep();
      },1000);
    };
    refresherStep();
    return list;
  };
});
