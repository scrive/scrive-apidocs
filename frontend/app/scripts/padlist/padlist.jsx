/** @jsx React.DOM */

define(['React', 'lists/list','archive/statustooltipmixin', 'moment', 'legacy_code'], function(React, List, StatusTooltipMixin, moment) {


var SelectPartyModal = function(signingIndexes,doc) {
          var self = this;
          self.current = 0;
          var modalContent = function() {
            var div = $("<div style='height:32px;'/>");
            var label = $("<label style='float:left;padding-right:10px;line-height: 32px;'>").text(localization.pad.giveForSigningThisDevice + " " );
            div.append(label);
            var options = [];
            for(var i=0;i<signingIndexes.length;i++) {
                var currentName = doc.field("subfields")[signingIndexes[i]].name.trim();
                if (i != self.current)
                options.push({
                  name: (currentName != "" ? currentName : localization.process.signatoryname + " " + (signingIndexes[i] + 1)),
                  value : i
                });
            }
            var currentName = doc.field("subfields")[signingIndexes[self.current]].name.trim();
            var select = new Select({
                name : ( currentName != "" ? currentName : localization.process.signatoryname + " " + (signingIndexes[self.current] + 1)),
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
                   url : "/padsign/" + doc.field("fields").id + "/" + doc.field("subfields")[signingIndexes[self.current]].id,
                   method : "POST"
                }).send();
            }
          });
};




return React.createClass({
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

      for(var i=0; i< doc.field("subfields").length; i++) {
        if (doc.field("subfields")[i].cansignnow == true &&  doc.field("subfields")[i].delivery =="pad")
          signingIndexes.push(i);
      }
      if (signingIndexes.length > 1) {
        new SelectPartyModal(signingIndexes,doc);
      }
      else {
        new Submit({
          url : "/padsign/" + doc.field("fields").id + "/" + doc.field("subfields")[signingIndexes[0]].id,
          method : "POST"
        }).send();
      }
    },
    openDeleteModal : function(docs) {
      var self = this;
      var confirmtext = $('<span />').html(localization.archive.documents.remove.body);
      var listElement = confirmtext.find('.put-one-or-more-things-to-be-deleted-here');
      if (docs.length == 1) {
        listElement.html($('<strong />').text(docs[0].field("fields").title));
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
            documentids: "[" + _.map(docs, function(doc){return doc.field("fields").id;}) + "]",
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
          url='/api/frontend/list?documentType=DocumentsForPad'
          dataFetcher={function(d) {return d.list;}}
          idFetcher={function(d) {return d.field("fields").id;}}
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
            select={true}
            width="30px"
          />
          <List.Column
            name={localization.archive.documents.columns.status}
            width="62px"
            rendering={function(d) {
              return (
                <div
                  onMouseOver={function(e) {self.showToolTip(d.field("fields").status,e)}}
                  onMouseOut={function() {self.hideToolTip()}}
                  className={"icon status "+d.field("fields").status}
                />);
            }}
          />

          <List.Column
            name={localization.archive.documents.columns.time}
            width="105px"
            sorting="time"
            rendering={function(d) {
              var time = moment(d.field("fields").time).toDate();
              return (<span title={time.fullTime()}>{time.toTimeAbrev()}</span>);
            }}
          />
          <List.Column
            name={localization.archive.documents.columns.party}
            width="210px"
            rendering={function(d) {
              return (
                <div onClick={function() {d.toggleExpand();}}>
                 {d.field("fields").party}
                </div>);
            }}
          />

          <List.Column
            name={localization.archive.documents.columns.title}
            width="230px"
            sorting="title"
            rendering={function(d) {
              return (<a className='s-archive-document-title' href='#' onClick={function() {self.openPadSigningView(d);return false;}}> {d.field("fields").title} </a>);
            }}
          />


          <List.Sublist
            count={function(d) {return d.field("subfields") != undefined ? d.field("subfields").length : 0}}
            rendering={function(d,i) {
              var time = d.field("subfields")[i].time ? moment(d.field("subfields")[i].time).toDate() : undefined;
              return [
                <td key="1"></td>,
                <td key="2">
                  <div
                    style={{"marginLeft":"10px"}}
                    className={"icon status "+d.field("subfields")[i].status}
                    onMouseOver={function(e) {self.showToolTip(d.field("subfields")[i].status,e)}}
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
                    {d.field("subfields")[i].name}
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



});
