var InfoTextInput = require("../common/infotextinput");
var Calendar = require("../../js/calendar.js");
var React = require("react");
var Utils = require("./utils");
var StatusTooltipMixin = require("./statustooltipmixin");
var List = require("../lists/list");
var DocumentColumns = require("./document_columns");
var DocumentFilters = require("./document_filters");
var jQuery = require("jquery");
var $ = require("jquery");
var Submit = require("../../js/submits.js").Submit;
var _ = require("underscore");
var FlashMessage = require("../../js/flashmessages.js").FlashMessage;
var Track = require("../common/track");
var DaysInputWithCalendar = require("../common/daysinputwithcalendar");
var TimeFilterOptionsMixin = require("./timefilteroptionsmixin");
var HtmlTextWithSubstitution = require("../common/htmltextwithsubstitution");
var Modal = require("../common/modal");

var SendReminderModalContent = React.createClass({
  propTypes: {
    documents: React.PropTypes.array
  },
  render: function () {
    if (this.props.documents === null) {
      return <p />;
    } else if (this.props.documents.length == 1) {
      return (
        <p>
          <HtmlTextWithSubstitution
            secureText={localization.archive.documents.sendreminder.bodysingle}
            lists={{
              ".put-document-name-here": {
                items: [this.props.documents[0].field("title")],
                wrapper: "<strong />"
              }
            }}
          />
        </p>
      );
    } else {
      return (
        <p>
          <HtmlTextWithSubstitution
            secureText={localization.archive.documents.sendreminder.bodymulti}
            subs={{
              ".put-number-of-documents-here": this.props.documents.length
            }}
          />
        </p>
      );
    }
  }
});

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

var ProlongModal = React.createClass({
  propTypes: {
    documents: React.PropTypes.array,
    active: React.PropTypes.bool.isRequired,
    onClose: React.PropTypes.func.isRequired,
    onAccept: React.PropTypes.func.isRequired,
  },
  getInitialState: function () {
    return {days: "1"};
  },
  onDaysInputChange: function (value) {
    this.setState({days: value});
  },
  onHide: function () {
    this.setState(this.getInitialState());
  },
  onAccept: function () {
    this.props.onAccept(this.state.days);
  },
  text: function () {
    if (this.props.documents === null) {
      return <span />;
    } else if (this.props.documents.length == 1) {
      return (
        <HtmlTextWithSubstitution
          secureText={localization.archive.documents.prolong.body}
          lists={{
            ".put-one-or-more-things-to-be-prolonged-here": {
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
          secureText={localization.archive.documents.prolong.body}
          subs={{
            ".put-one-or-more-things-to-be-prolonged-here": sub
          }}
        />
      );
    }
  },
  render: function () {
    return (
      <Modal.Container
        active={this.props.active}
        onHide={this.onHide}
      >
        <Modal.Header
          title={localization.archive.documents.prolong.action}
          showClose={true}
          onClose={this.props.onClose}
        />
        <Modal.Content>
          <div className="prolongmodal">
            {this.text()}
            <DaysInputWithCalendar
              ref="reminderEditor"
              infotext="-"
              label={localization.prolongmodal.days}
              labelClassName="archive-prolong-modal-calendar-label"
              days={this.state.days}
              canBeEmpty={false}
              minDays={1}
              maxDays={365}
              onChange={this.onDaysInputChange}
            />
          </div>
        </Modal.Content>
        <Modal.Footer>
          <Modal.CancelButton onClick={this.props.onClose} />
          <Modal.AcceptButton
            type="cancel"
            text={localization.archive.documents.prolong.action}
            onClick={this.onAccept}
          />
        </Modal.Footer>
      </Modal.Container>
    );
  }
});

module.exports = React.createClass({
    mixins: [StatusTooltipMixin, List.ReloadableContainer, TimeFilterOptionsMixin],
    getInitialState: function () {
      return {
        showSendReminderModal: false,
        documentsToRemind: null,
        documentsToProlong: null,
        showCancelModal: false,
        documentsToCancel: null,
        showRemoveModal: false,
        showProlongModal: false,
        documentsToRemove: null
      };
    },
    openSendReminderModal : function(selected) {
      this.setState({
        showSendReminderModal: true,
        documentsToRemind: selected
      });
    },
    onSendReminderModalClose: function () {
      this.setState({
        showSendReminderModal: false,
        documentsToRemind: null
      });
    },
    onSendReminderModalAccept: function () {
      var self = this;
      var documentIds = _.map(
        this.state.documentsToRemind,
        function (doc) {
          return doc.field("id");
        }
      );

      Track.track('Send reminder');
      new Submit({
        url: "/d/remind",
        method: "POST",
        documentids: "[" + documentIds + "]",
        ajaxsuccess : function() {
          new FlashMessage({
            type: 'success',
            content: localization.archive.documents.sendreminder.successMessage
          });

          self.reload();
          self.onSendReminderModalClose();
        },
        ajaxerror : function() {
          new FlashMessage({
            type: 'error',
            content: localization.archive.documents.sendreminder.errorMessage
          });

          self.reload();
          self.onSendReminderModalClose();
        }
      }).sendAjax();
    },
    openCancelModal : function(selected) {
      this.setState({
        showCancelModal: true,
        documentsToCancel: _.map(
          selected,
          function (doc) {
            return doc.field("id");
          }
        )
      });
    },
    onCancelModalClose: function () {
      this.setState({
        showCancelModal: false,
        documentsToCancel: null
      });
    },
    onCancelModalAccept: function () {
      var self = this;

      Track.track('Cancel document');
      new Submit({
        url: "/d/cancel",
        method: "POST",
        documentids: "[" + this.state.documentsToCancel + "]",
        ajaxsuccess : function() {
          new FlashMessage({
            type: 'success',
            content: localization.archive.documents.cancel.successMessage
          });

          self.reload();
          self.onCancelModalClose();
        },
        ajaxerror : function() {
          self.reload();
          self.onCancelModalClose();
        }
      }).sendAjax();
    },
    openRemoveModal : function(selected) {
      this.setState({
        showRemoveModal: true,
        documentsToRemove: selected
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
            type: 'success',
            content: localization.archive.documents.remove.successMessage
          });

          self.reload();
          self.onRemoveModalClose();
        }
      }).sendAjax();
    },
    openProlongModal: function (selected) {
      this.setState({showProlongModal: true,
                     documentsToProlong: selected});
    },
    onProlongModalClose: function () {
      this.setState({showProlongModal: false,
                     documentsToProlong: null});
    },
    onProlongModalAccept: function (days) {
      var self = this;
      var documentIds = _.map(this.state.documentsToProlong, function (doc) {
        return doc.field("id");
      });

      Track.track('Prolong documents');
      new Submit({
        url: "/d/prolong",
        method: "POST",
        documentids: "[" + documentIds + "]",
        days: days,
        ajaxsuccess : function() {
          new FlashMessage({
            type: 'success',
            content: localization.archive.documents.prolong.successMessage
          });

          self.reload();
          self.onProlongModalClose();
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
                {"filter_by" : "is_not_template"},
                {"filter_by" : "is_not_in_trash"},
                {"filter_by" : "is_not_part_of_a_flow"}
              ])}
            dataFetcher={Utils.dataFetcher}
            idFetcher={Utils.idFetcher}
            loadLater={self.props.loadLater}
            ref='list'
          >
            <List.TextFiltering text={localization.archive.documents.search} />

            <List.ListAction
              name={localization.archive.documents.sendreminder.action}
              onSelect={function(selected,model) {
                if (selected.length ==0 ) {
                  new FlashMessage({type: 'error', content: localization.archive.documents.sendreminder.emptyMessage});
                } else {
                  var allSelectedAreRemindable = _.all(selected, function(doc) {
                    var isPending = doc.field("status") === "pending";
                    var signingParties = Utils.signingParties(doc);
                    var signingPartiesWithoutDeliveryProblems = _.filter(signingParties, function (s) {
                      return s.email_delivery_status !== "not_delivered" && s.mobile_delivery_status !== "not_delivered";
                    });
                    var deliverableSigningPartiesWithoutDeliveryProblems = _.filter(signingPartiesWithoutDeliveryProblems,
                                                                                    function (s) {
                      return s.delivery_method !== "api" && s.delivery_method !== "pad";
                    });
                    return isPending && deliverableSigningPartiesWithoutDeliveryProblems.length > 0;
                  });
                  if (!allSelectedAreRemindable) {
                    new FlashMessage({type: 'error', content: localization.archive.documents.sendreminder.notAvailableMessage});
                  } else {
                    self.openSendReminderModal(selected);
                  }
                }
              }}
            />

            <List.ListAction
              name={localization.archive.documents.cancel.action}
              onSelect={function(selected,model) {
                if (selected.length ==0 ) {
                  new FlashMessage({type: 'error', content: localization.archive.documents.cancel.emptyMessage});
                } else {
                  var allCanBeCancelled = _.all(selected, function(doc) {
                    return doc.field("status") == "pending" &&
                      (Utils.viewerIsAuthor(doc) || (doc.field("viewer").role == "company_admin"  && self.props.forCompanyAdmin)
                    );
                  });
                  if (!allCanBeCancelled) {
                    new FlashMessage({type: 'error', content: localization.archive.documents.cancel.notAvailableMessage});
                  } else {
                    self.openCancelModal(selected);
                  }
                }
              }}
            />

            <List.ListAction
              name={localization.archive.documents.remove.action}
              onSelect={function(selected,model) {
                if (selected.length ==0 ) {
                  new FlashMessage({type: 'error', content: localization.archive.documents.cancel.emptyMessage});
                } else {
                  var canDeleteAll = _.all(selected, function (doc) {
                    return (doc.field("status") !== "pending" ||
                      (Utils.viewerIsAuthor(doc) || (doc.field("viewer").role == "company_admin"  && self.props.forCompanyAdmin)) ||
                      (Utils.currentViewerParty(doc) && Utils.signatoryCanSignNow(doc,Utils.currentViewerParty(doc)))
                    );
                  });
                  if (!canDeleteAll) {
                    new FlashMessage({type: 'error', content: localization.archive.documents.remove.invalidMessage});
                  } else {
                    self.openRemoveModal(selected);
                  }
                }
              }}
            />

            <List.ListAction
              name={localization.archive.documents.prolong.action}
              onSelect={function(selected,model) {
                if (selected.length ==0 ) {
                  new FlashMessage({type: 'error', content: localization.archive.documents.prolong.emptyMessage});
                } else {
                  var canProlongAll = _.all(selected, function (doc) {
                    return ((doc.field("status") === "timedout") &&
                            (Utils.viewerIsAuthor(doc) || (doc.field("viewer").role == "company_admin"  && self.props.forCompanyAdmin)));
                  });
                  if (!canProlongAll) {
                    new FlashMessage({type: 'error', content: localization.archive.documents.prolong.invalidMessage});
                  } else {
                    self.openProlongModal(selected);
                  }
                }
              }}
            />

            <List.ListSelectActions
              name={localization.more}
              width={270}
              actions={[
                {
                  name: localization.archive.documents.csv.action,
                  onSelect : function(listmodel) {
                    Track.track('Download CSV');
                    var url = "/d/csv?";
                    var params =  listmodel.urlParams();
                    _.each(params,function(a,b){url+=(b+"="+a+"&");});
                    window.location.href = url;
                    return true;
                  }
                },
                {
                  name: localization.archive.documents.zip.action,
                  onSelect : function(listmodel) {
                    Track.track('Download PDFs');
                    var selected = listmodel.list().getSelected();
                    if (selected.length == 0 ) {
                      new FlashMessage({type: 'error', content : localization.archive.documents.zip.emptyMessage});
                      return true;
                    }
                    else if (selected.length == 1) {
                      var url =  "/api/frontend/documents/" + selected[0].field("id") + "/files/main/" + encodeURIComponent(selected[0].field("title")) + ".pdf";
                      window.location.href = url;
                      return true;
                    } else {
                      var url =  "/d/zip?";
                      url += "documentids=[" + _.map(selected,function(s){return s.field("id");}) + "]";
                      window.location.href = url;
                      return true;
                    }
                  }
                }
              ]}
           />

            {DocumentFilters({list: self, fromToFilterOptions: self._fromToFilterOptions})}
            {!self.props.hasDataRetentionPolicy ? "" :
              <List.ListSubHeader>
                {localization.archive.documents.dataRetentionPolicyRemark}
              </List.ListSubHeader>}
            {DocumentColumns({list:self})}
             <List.ListFooter>
               <div className='table-statuses'>
                 <div className='icon status float-left draft first'/>
                 <div className='float-left'>{localization.archive.documents.statusDescription.draft}</div>
                 <div className='icon status float-left problem'/>
                 <div className='float-left'>{localization.archive.documents.statusDescription.cancelled}</div>
                 <div className='icon status float-left sent'/>
                 <div className='float-left'>{localization.archive.documents.statusDescription.sent}</div>
                 <div className='icon status float-left delivered'/>
                 <div className='float-left'>{localization.archive.documents.statusDescription.delivered}</div>
                 <div className='icon status float-left read'/>
                 <div className='float-left'>{localization.archive.documents.statusDescription.read}</div>
                 <div className='icon status float-left opened'/>
                 <div className='float-left'>{localization.archive.documents.statusDescription.opened}</div>
                 <div className='icon status float-left signed'/>
                 <div className='float-left'>{localization.archive.documents.statusDescription.signed}</div>
               </div>
            </List.ListFooter>
            <List.Pagination/>
          </List.List>

          <Modal.Container active={self.state.showSendReminderModal}>
            <Modal.Header
              title={localization.archive.documents.sendreminder.action}
              showClose={true}
              onClose={self.onSendReminderModalClose}
            />
            <Modal.Content>
              <SendReminderModalContent documents={self.state.documentsToRemind} />
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton onClick={self.onSendReminderModalClose} />
              <Modal.AcceptButton
                text={localization.archive.documents.sendreminder.action}
                onClick={self.onSendReminderModalAccept}
              />
            </Modal.Footer>
          </Modal.Container>

          <Modal.Container active={self.state.showCancelModal}>
            <Modal.Header
              title={localization.archive.documents.cancel.action}
              showClose={true}
              onClose={self.onCancelModalClose}
            />
            <Modal.Content>
              <p>{localization.archive.documents.cancel.body}</p>
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton onClick={self.onCancelModalClose} />
              <Modal.AcceptButton
                onClick={self.onCancelModalAccept}
              />
            </Modal.Footer>
          </Modal.Container>

          <Modal.Container active={self.state.showRemoveModal}>
            <Modal.Header
              title={localization.archive.documents.remove.action}
              showClose={true}
              onClose={self.onRemoveModalClose}
            />
            <Modal.Content>
              <RemoveModalContent documents={self.state.documentsToRemove} />
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton onClick={self.onRemoveModalClose} />
              <Modal.AcceptButton
                type="cancel"
                text={localization.archive.documents.remove.action}
                onClick={self.onRemoveModalAccept}
              />
            </Modal.Footer>
          </Modal.Container>

          <ProlongModal
            documents={self.state.documentsToProlong}
            active={self.state.showProlongModal}
            onClose={self.onProlongModalClose}
            onAccept={self.onProlongModalAccept}
          />
        </div>
      );
    }
});
