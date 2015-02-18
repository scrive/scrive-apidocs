/*
 * Design view tabs
 *
 */

define(['Backbone', 'legacy_code'], function() {

window.DesignViewTabsView = function(args) {
  var model = args.model;
  var document = model.document();
  var participantsView = new DesignViewParticipantsView({ model : model});
  var draggablesView   = new DesignViewDraggablesView({ model : model});
  var processView      = new DesignViewProcessView({ model : model });
  var tab1Name = $("<div class='design-view-tab-text'/>")
                      .text(localization.designview.editParticipants);
  var tab2Name = $("<div class='design-view-tab-text'/>")
                      .text(localization.designview.editDocument);
  var tab3Name = $("<div class='design-view-tab-text'/>")
                      .text(localization.designview.editSigningProcess);

  var tab1 = new Tab({
          name: tab1Name,
          pagehash : "participants",
          elems: [ $("<div class='design-view-tab-center' />").append($(participantsView.el))],
          onActivate : function() {
               mixpanel.track('Click tab', {
                        Action: 'Open',
                        Tab: '1'
                    });
          },
          onShow: function() {
               participantsView.initCustomScrollBox();
          },
          onHide : function() {
               participantsView.closeAllParticipants();
          }
          });
  var tab2 =  new Tab({
          name: tab2Name,
          pagehash : "placements",
          available : document.mainfile() != undefined,
          elems: [ $("<div class='design-view-tab-center' />").append($(draggablesView.el))],
          onActivate : function() {
               mixpanel.track('Click tab', {
                        Action: 'Open',
                        Tab: '2'
                    });
         }
  });
  var tab3 = new Tab({
          name: tab3Name,
          pagehash : "process",
          elems: [ $("<div class='design-view-tab-center' />").append($(processView.el))],
          onShow : function() {
              processView.rerenderMiddleColumn();
            },
          onActivate : function() {
               mixpanel.track('Click tab', {
                        Action: 'Open',
                        Tab: '3'
                    });
          }
  });

  var tabs = new KontraTabs({
      numbers : false,
      tabs: [tab1,tab2,tab3],
      tabsTail : $(),
      canHaveNoActiveTab : true,
      slideEffect : true,
      beforeEachChange : function() {
        processView.hideAllCalendars();
      }
  });
  document.bind('change:ready change:file', function() {
          tab2.setAvailable(document.mainfile() != undefined);
          if (document.ready()) {
              if (document.mainfile() == undefined)
                tabs.deactive();
              else {
                var updateOnReady = function() {
                  if (document.mainfile().ready()) {
                     setTimeout(function() {
                        tabs.activate(tabs.activeTab() || tab1);
                        model.trigger('visibility:designviewtab');
                    }, 800);
                  }
                };
                document.mainfile().bind('change', updateOnReady);
              }
          }
       });
  tab2.setAvailable(document.mainfile() != undefined);
  this.el = function() { return tabs.el();};

};

});
