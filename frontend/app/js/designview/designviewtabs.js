/*
 * Design view tabs
 *
 */

define(['Backbone',  'React',  'designview/participants/participants' , 'designview/processsettings/processsettings' , 'legacy_code'], function(Backbone, React, Participants, ProcessSettings) {

window.DesignViewTabsView = function(args) {
  var model = args.model;
  var document = model.document();
  console.log("Preparing new tab");
  var participantsViewEl = $("<div/>");
  var participantsView = React.render(React.createElement(Participants,{
    model: model
  }), participantsViewEl[0]);

  //var participantsView = new DesignViewParticipantsView({ model : model});
  var draggablesView   = new DesignViewDraggablesView({ model : model});
  var processSettingsEl      = $("<div/>");
  var processSettings = React.render(React.createElement(ProcessSettings,{
    model: model
  }), processSettingsEl[0]);
  var tab1Name = $("<div class='design-view-tab-text'/>")
                      .text(localization.designview.editParticipants);
  var tab2Name = $("<div class='design-view-tab-text'/>")
                      .text(localization.designview.editDocument);
  var tab3Name = $("<div class='design-view-tab-text'/>")
                      .text(localization.designview.editSigningProcess);

  var tab1 = new Tab({
          name: tab1Name,
          pagehash : "participants",
          elems: [ $("<div class='design-view-tab-center' />").append(participantsViewEl)],
          onActivate : function() {
               mixpanel.track('Click tab', {
                        Action: 'Open',
                        Tab: '1'
                    });
          },
          onShow: function() {
            participantsView.forceUpdate();
               //participantsView.initCustomScrollBox();
          },
          onHide : function() {
               //participantsView.closeAllParticipants();
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
          elems: [ $("<div class='design-view-tab-center' />").append(processSettingsEl)],
          onShow: function() {
               processSettings.forceUpdate();
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
        processSettings.hideAllCalendars();
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
