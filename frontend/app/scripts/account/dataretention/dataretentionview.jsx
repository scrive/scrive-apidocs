import React from "react";
import classNames from "classnames";
import FlashMessage from "../../../js/flashmessages";
import BackboneMixin from "../../common/backbone_mixin";
import Button from "../../common/button";
import DataRetention from "./dataretention";
import IdleDocTimeoutOption from "./idledoctimeoutoption";

module.exports = React.createClass({
  mixins: [BackboneMixin.BackboneMixin],

  getInitialState: function () {
    return {
      model: new DataRetention()
    };
  },

  getBackboneModels: function () {
    return [this.state.model];
  },

  onSaveButtonClick: function () {
    this.state.model.save(function () {
      new FlashMessage.FlashMessage({
        type: "success",
        content: localization.account.dataRetention.successMessage
      });

    }, function (response) {
      const msg = response.responseJSON.error_message;
      new FlashMessage.FlashMessage({type: "error", content: msg});
    });
  },

  onIdleDocTimeoutChange: function(status) {
    var self = this;
    return function(value) {
      self.state.model.setIdleDocTimeout(status, value);
    };
  },

  onImmediateTrashCheckBoxClick: function () {
    this.state.model.toggleImmediateTrash();
  },

  render: function () {
    const buttonClassName = classNames("save", {
      inactive: !this.state.model.isValid()
    });

    const immediateTrashCheckboxClassName = classNames("checkbox", {
      checked: this.state.model.immediateTrash(),
      disabled: this.state.model.companyImmediateTrash()
    });

    return (
      <div className="tab-container">
        <div className="tab-content data-retention">
          <h1 className="data-retention-header">
            {localization.account.dataRetention.title}
          </h1>

          <div className="data-retention-form">
            <IdleDocTimeoutOption
              model={this.state.model}
              status="preparation"
              text={localization.account.dataRetention.preparation.text}
              description={localization.account.dataRetention.preparation.description}
            />

            <IdleDocTimeoutOption
              model={this.state.model}
              status="closed"
              text={localization.account.dataRetention.closed.text}
              description={localization.account.dataRetention.closed.description}
            />

            <IdleDocTimeoutOption
              model={this.state.model}
              status="canceled"
              text={localization.account.dataRetention.canceled.text}
              description={localization.account.dataRetention.canceled.description}
            />

            <IdleDocTimeoutOption
              model={this.state.model}
              status="timedout"
              text={localization.account.dataRetention.timedout.text}
              description={localization.account.dataRetention.timedout.description}
            />

            <IdleDocTimeoutOption
              model={this.state.model}
              status="rejected"
              text={localization.account.dataRetention.rejected.text}
              description={localization.account.dataRetention.rejected.description}
            />

            <IdleDocTimeoutOption
              model={this.state.model}
              status="error"
              text={localization.account.dataRetention.error.text}
              description={localization.account.dataRetention.error.description}
            />

           <div className="data-retention-option-wrapper">
             <div className="data-retention-option-checkbox">
                <div
                  className={immediateTrashCheckboxClassName}
                  onClick={this.onImmediateTrashCheckBoxClick}
                  >
                    <div className="checkmark" />
                </div>
              </div>
                <div className="data-retention-option-main-text">
                  {localization.account.dataRetention.immediateTrash.text}
                </div>
                <div className="data-retention-option-description">
                  {localization.account.dataRetention.immediateTrash.description}
                </div>
            </div>

          </div>

          <div className="data-retention-sidebar">
            <p>{localization.account.dataRetention.sideBar.paragraph1}</p>
            <p>{localization.account.dataRetention.sideBar.paragraph2}</p>
            <p>{localization.account.dataRetention.sideBar.paragraph3}</p>
            <p>{localization.account.dataRetention.sideBar.paragraph4}</p>
          </div>

          <div className="clearfix"></div>

          <div className="data-retention-footer">
            <Button
              ref="save"
              type="action"
              size="small"
              className={buttonClassName}
              text={localization.account.dataRetention.save}
              onClick={this.onSaveButtonClick}
            />
          </div>
        </div>
      </div>
    );
  }
});
