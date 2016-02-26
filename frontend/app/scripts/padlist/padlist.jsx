var React = require("react");
var Utils = require("../archive/utils");
var List = require("../lists/list");
var StatusTooltipMixin = require("../archive/statustooltipmixin");
var Select = require("../common/select");
var moment = require("moment");
var $ = require("jquery");
var Confirmation = require("../../js/confirmations.js").Confirmation;
var Submit = require("../../js/submits.js").Submit;
var LocalStorage = require("../../js/storage.js").LocalStorage;
var _ = require("underscore");
var FlashMessage = require("../../js/flashmessages.js").FlashMessage;



var SelectPartyModal = function(signingIndexes,doc) {
          var self = this;
          self.current = 0;
          var modalContent = function() {
            var div = $("<div style='height:32px;'/>");
            var label = $("<label style='float:left;padding-right:10px;line-height: 32px;'>").text(localization.pad.giveForSigningThisDevice + " " );
            div.append(label);
            var options = [];
            for(var i=0;i<signingIndexes.length;i++) {
                var currentName = Utils.signatorySmartName(doc.field("parties")[signingIndexes[i]]).trim();
                if (i != self.current)
                options.push({
                  name: (currentName != "" ? currentName : localization.process.signatoryname + " " + (signingIndexes[i] + 1)),
                  value : i
                });
            }
            var currentName = Utils.signatorySmartName(doc.field("parties")[signingIndexes[self.current]]).trim();

            var $select1 = $("<span>");
            React.render(React.createElement(Select, {
                name : ( currentName != "" ? currentName : localization.process.signatoryname + " " + (signingIndexes[self.current] + 1)),
                className : "float-left",
                options : options,
                onSelect : function(v) {
                  self.current = v;
                  self.onChange();
                  return true;
                }
            }), $select1[0]);
            return div.append($select1[0]);
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
                   url : "/padsign/" + doc.field("id") + "/" + doc.field("parties")[signingIndexes[self.current]].id,
                   method : "POST"
                }).send();
            }
          });
};




module.exports = React.createClass({
    getInitialState: function() {
      return {currentCounter: 1, currentInterval : 1};
    },
    mixins : [List.ReloadableContainer,StatusTooltipMixin],
    resetCounter : function() {
      this.setState({
       currentCounter : 1,
       currentInterval : 1
      });
    },
    refreshStep : function() {
      var self = this;
      if (this.refs.list == undefined) return;
      if (this.state.currentCounter <= 0) {
          self.refs.list.checkIfChangedAndCallback(function() { self.reload(); self.resetCounter(); });
          var newInterval =  Math.min(this.state.currentInterval + 1,30);
          this.setState({
            currentCounter : newInterval,
            currentInterval : newInterval
          });
      } else {
        this.setState({currentCounter : this.state.currentCounter - 1});
      }
    },
    openPadSigningView : function(doc) {
      var signingIndexes = [];
      LocalStorage.set("backlink","target","to-sign");

      for(var i=0; i< doc.field("parties").length; i++) {
        if (Utils.signatoryCanSignNow(doc, doc.field("parties")[i]) &&  doc.field("parties")[i].delivery_method =="pad")
          signingIndexes.push(i);
      }
      if (signingIndexes.length > 1) {
        new SelectPartyModal(signingIndexes,doc);
      }
      else {
        new Submit({
          url : "/padsign/" + doc.field("id") + "/" + doc.field("parties")[signingIndexes[0]].id,
          method : "POST"
        }).send();
      }
    },
    openDeleteModal : function(docs) {
      var self = this;
      var confirmtext = $('<span />').html(localization.archive.documents.remove.body);
      var listElement = confirmtext.find('.put-one-or-more-things-to-be-deleted-here');
      if (docs.length == 1) {
        listElement.html($('<strong />').text(docs[0].field("title")));
      } else {
        listElement.text(docs.length + (" " + localization.documents).toLowerCase());
      }
      var confirmationPopup = new Confirmation({
        acceptText: localization.archive.documents.remove.action,
        rejectText: localization.cancel,
        title: localization.archive.documents.remove.action,
        content: confirmtext,
        onAccept : function() {
          mixpanel.track('Delete document');
          new Submit({
            url: "/d/delete",
            method: "POST",
            documentids: "[" + _.map(docs, function(doc){return doc.field("id");}) + "]",
            ajaxsuccess : function() {
              new FlashMessage({type : "success", content : localization.archive.documents.remove.successMessage});
              self.reload();
              confirmationPopup.clear();
            }
          }).sendAjax();
        }
      });
      return true;
    },
    render: function() {
      var self = this;
      return (
        <List.List
          maxPageSize={Utils.maxPageSize}
          totalCountFunction={Utils.totalCountFunction}
          url={Utils.listCallUrl}
          paramsFunction={Utils.paramsFunctionWithFilter([
              {"filter_by" : "trash", "is_trashed" : false},
              {"filter_by" : "template", "is_template" : false},
              {"filter_by" : "is_signable_on_pad"},
              {"filter_by" : "is_author"}
            ])}
          dataFetcher={Utils.dataFetcher}
          idFetcher={Utils.idFetcher}
          loadLater={self.props.loadLater}
          minRows={8}
          ref='list'
        >

        <List.TextFiltering text={localization.archive.documents.search} />

          <List.ListAction
            name={localization.archive.documents.remove.action}
            className="float-left"
            onSelect={function(selected,model) {
              if (selected.length ==0 ) {
                new FlashMessage({type : "error", content: localization.archive.documents.cancel.emptyMessage});
                return false;
              }
              self.openDeleteModal(selected);
            }}
          />
          <List.ListAction
            name={localization.padlist.updateAction}
            className="float-left"
            onSelect={function() {
              self.reload();
              self.resetCounter();
            }}
          />

          <List.Column
            key="checkbox"
            select={true}
            width="30px"
          />
          <List.Column
            key="status"
            name={localization.archive.documents.columns.status}
            width="62px"
            rendering={function(d) {
              var status = Utils.documentStatus(d);
              return (
                <div
                  onMouseOver={function(e) {self.showToolTip(status,e);}}
                  onMouseOut={function() {self.hideToolTip();}}
                  className={"icon status "+status}
                />);
            }}
          />

          <List.Column
            key="time"
            name={localization.archive.documents.columns.time}
            width="105px"
            sorting="mtime"
            rendering={function(d) {
              var time = moment(d.field("mtime")).toDate();
              return (<span title={time.fullTime()}>{time.toTimeAbrev()}</span>);
            }}
          />
          <List.Column
            key="party"
            name={localization.archive.documents.columns.party}
            width="210px"
            rendering={function(d) {
              return (
                <div onClick={function() {d.toggleExpand();}}>
                 {Utils.documentParty(d)}
                </div>);
            }}
          />

          <List.Column
            key="title"
            name={localization.archive.documents.columns.title}
            width="230px"
            sorting="title"
            rendering={function(d) {
              return (<a className='s-archive-document-title' href='#' onClick={function() {self.openPadSigningView(d);return false;}}> {d.field("title")} </a>);
            }}
          />


          <List.Sublist
            key="signatories"
            count={function(d) {
              if (d.field("status") == "preparation") {
                return 0;
              } else {
                return _.filter(d.field("parties"), function(s) { return s.is_signatory;}).length;
              }
            }}
            rendering={function(d,i) {
              var signatory = _.filter(d.field("parties"), function(s) { return s.is_signatory;})[i];
              var time = Utils.signatoryTime(signatory) && moment(Utils.signatoryTime(signatory)).toDate();
              return [
                <td key="1"></td>,
                <td key="2">
                  <div
                    style={{"marginLeft":"10px"}}
                    className={"icon status "+Utils.signatoryStatus(d,signatory)}
                    onMouseOver={function(e) {self.showToolTip(Utils.signatoryStatus(d,signatory),e)}}
                    onMouseOut={function() {self.hideToolTip()}}
                  />
                </td>,
                <td key="3">
                  { /* if */ (time != undefined) &&
                    (<div style={{"marginLeft":"10px"}}  title={time.fullTime()}>
                      {time.toTimeAbrev()}
                     </div>)
                  }
                </td>,
                <td key="4">
                  <div style={{"marginLeft":"10px"}}>
                    {Utils.signatorySmartName(signatory)}
                  </div>
                </td>,
                <td key="5"></td>
              ];
            }}
          />
          <List.Pagination showOnlyForMultiplePages={true} />
        </List.List>
      );
    }
});
