var React = require("react");
var Utils = require("../archive/utils");
var List = require("../lists/list");
var StatusTooltipMixin = require("../archive/statustooltipmixin");
var Select = require("../common/select");
var Track = require("../common/track");
var moment = require("moment");
var $ = require("jquery");
var Submit = require("../../js/submits.js").Submit;
var LocalStorage = require("../../js/storage.js").LocalStorage;
var _ = require("underscore");
var FlashMessage = require("../../js/flashmessages.js").FlashMessage;

var HtmlTextWithSubstitution = require("../common/htmltextwithsubstitution");
var Modal = require("../common/modal");

var RemoveModalContent = React.createClass({
  propTypes: {
    documents: React.PropTypes.array
  },
  render: function () {
    if (this.props.documents === null) {
      return <span />;
    } else if (this.props.documents.length == 1) {
      return (
        <HtmlTextWithSubstitution
          secureText={localization.archive.documents.remove.body}
          lists={{
            ".put-one-or-more-things-to-be-deleted-here": {
              items: [this.props.documents[0].field("title")],
              wrapper: "<strong />"
            }
          }}
        />
      );
    } else {
      var sub = this.props.documents.length + (" " + localization.documents).toLowerCase();
      return (
        <HtmlTextWithSubstitution
          secureText={localization.archive.documents.remove.body}
          subs={{
            ".put-one-or-more-things-to-be-deleted-here": sub
          }}
        />
      );
    }
  }
});

var SelectPadSigningPartyModalContent = React.createClass({
  propTypes: {
    parties: React.PropTypes.array,
    selectedParty: React.PropTypes.object,
    onPartySelect: React.PropTypes.func.isRequired
  },
  onPartySelect: function (value) {
    var selectedParty = _.filter(this.props.parties, function (item) {
      return item.value == value;
    });

    this.props.onPartySelect(selectedParty[0]);
  },
  render: function () {
    if (!this.props.parties) {
      return (<div></div>);
    } else {
      var self = this;

      return (
        <div style={{height: 32}}>
          <label style={{float: "left", paddingRight: 10, lineHeight: "32px"}}>
            {localization.pad.giveForSigningThisDevice}
          </label>
          <Select
            className="float-left"
            options={this.props.parties}
            isOptionSelected={function (option) {
              return (self.props.selectedParty && option.value == self.props.selectedParty.value);
            }}
            onSelect={this.onPartySelect}
          />
        </div>
      );
    }
  }
})

module.exports = React.createClass({
    getInitialState: function() {
      return {
        currentCounter: 1,
        currentInterval : 1,
        showRemoveModal: false,
        documentsToRemove: null,
        showSelectPadSigningPartyModal: false,
        padSigningParties: null,
        selectedPadSigningParty: null
      };
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
    openPadSigningView : function (doc) {
      LocalStorage.set("backlink","target","to-sign");

      var padSigningParties = [];
      _.each(doc.field("parties"), function (party, index) {
        if (Utils.signatoryCanSignNow(doc, party) && party.delivery_method == "pad") {
          var partyName = Utils.signatorySmartName(party).trim();
          if (partyName == "") {
            partyName = localization.process.signatoryname + " " + (index + 1);
          }

          padSigningParties.push({
            name: partyName,
            document: doc,
            value: index
          });
        }
      });

      if (padSigningParties.length > 1) {
        this.setState({
          showSelectPadSigningPartyModal: true,
          padSigningParties: padSigningParties,
          selectedPadSigningParty: padSigningParties[0]
        });
      } else {
        this.startPadSigning(padSigningParties[0]);
      }
    },
    onSelectPadSigningPartyModalClose: function () {
      this.setState({
        showSelectPadSigningPartyModal: false,
        padSigningParties: null,
        selectedPadSigningParty: null
      });
    },
    onSelectPadSigningPartyModalAccept: function () {
      if (this.state.selectedPadSigningParty) {
        Track.track("Give for pad signing to some pad signatory - opening signview -from list");
        this.startPadSigning(this.state.selectedPadSigningParty);
      }
    },
    onSelectPadSigningPartyModalPartySelectParty: function (party) {
      this.setState({selectedPadSigningParty: party});
    },
    startPadSigning: function (party) {
      var signatory = party.document.field("parties")[party.value];

      new Submit({
        url: "/padsign/" + party.document.field("id") + "/" + signatory.id,
        method : "POST"
      }).send();
    },
    openDeleteModal : function(docs) {
      this.setState({
        showRemoveModal: true,
        documentsToRemove: docs
      });
    },
    onRemoveModalClose: function () {
      this.setState({
        showRemoveModal: false,
        documentsToRemove: null
      });
    },
    onRemoveModalAccept: function () {
      var self = this;
      var documentIds = _.map(
        this.state.documentsToRemove,
        function (doc) {
          return doc.field("id");
        }
      );

      Track.track('Delete document');
      new Submit({
        url: "/d/delete",
        method: "POST",
        documentids: "[" + documentIds + "]",
        ajaxsuccess : function() {
          new FlashMessage({
            type: "success",
            content: localization.archive.documents.remove.successMessage
          });

          self.reload();
          self.onRemoveModalClose();
        }
      }).sendAjax();
    },
    render: function() {
      var self = this;
      return (
        <div>
          <List.List
            maxPageSize={Utils.maxPageSize}
            totalCountFunction={Utils.totalCountFunction}
            url={Utils.listCallUrl}
            paramsFunction={Utils.paramsFunctionWithFilter([
                {"filter_by" : "is_not_in_trash"},
                {"filter_by" : "is_not_template"},
                {"filter_by" : "is_signable_on_pad"},
                {"filter_by" : "is_author"},
                {"filter_by" : "is_not_part_of_a_flow"}
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

          <Modal.Container active={this.state.showRemoveModal}>
            <Modal.Header
              title={localization.archive.documents.remove.action}
              showClose={true}
              onClose={this.onRemoveModalClose}
            />
            <Modal.Content>
              <RemoveModalContent documents={this.state.documentsToRemove} />
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton onClick={this.onRemoveModalClose} />
              <Modal.AcceptButton
                title={localization.archive.documents.remove.action}
                onClick={this.onRemoveModalAccept}
              />
            </Modal.Footer>
          </Modal.Container>

          <Modal.Container active={this.state.showSelectPadSigningPartyModal}>
            <Modal.Header
              title={localization.authorview.goToSignView}
              showClose={true}
              onClose={this.onSelectPadSigningPartyModalClose}
            />
            <Modal.Content>
              <SelectPadSigningPartyModalContent
                parties={this.state.padSigningParties}
                selectedParty={this.state.selectedPadSigningParty}
                onPartySelect={this.onSelectPadSigningPartyModalPartySelectParty}
              />
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton onClick={this.onSelectPadSigningPartyModalClose} />
              <Modal.AcceptButton onClick={this.onSelectPadSigningPartyModalAccept} />
            </Modal.Footer>
          </Modal.Container>
        </div>
      );
    }
});
