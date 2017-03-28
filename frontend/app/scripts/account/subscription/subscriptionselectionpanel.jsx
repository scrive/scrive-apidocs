var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");
var Button = require("../../common/button");
var Submit = require("../../../js/submits.js").Submit;
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var classNames = require("classnames");
var InfoTextInput = require("../../common/infotextinput");
var EmailValidation = require("../../../js/validation.js").EmailValidation;

module.exports = React.createClass({
    mixins: [React.addons.LinkedStateMixin],
    propTypes: {
      fstname: React.PropTypes.string,
      sndname: React.PropTypes.string,
      email: React.PropTypes.string
    },
    getDefaultProps: function () {
      return {
        fstname: "",
        sndname: "",
        email: ""
      };
    },
    getInitialState: function () {
      return {
        mode: "one_year",
        expanded_plan: null,
        fstname: this.props.fstname,
        sndname: this.props.sndname,
        email: this.props.email,
        message: ""
      };
    },
    oneYearMode: function () {
      return this.state.mode == "one_year";
    },
    switchToOneYearMode: function () {
      this.setState({mode: "one_year",  expanded_plan: null});
    },
    twoYearsMode: function () {
      return this.state.mode == "two_years";
    },
    switchToTwoYearsMode: function () {
      this.setState({mode: "two_years",  expanded_plan: null});
    },
    expandOnOnePlan: function () {
      this.setState({expanded_plan: "one"});
    },
    expandOnTeamPlan: function () {
      this.setState({expanded_plan: "team"});
    },
    expandOnCompanyPlan: function () {
      this.setState({expanded_plan: "company"});
    },
    expandOnEnterprisePlan: function () {
      this.setState({expanded_plan: "enterprise"});
    },
    expandOnNothing: function () {
      this.setState({expanded_plan: null});
    },
    expandedOnAnything: function () {
      return this.state.expanded_plan != null;
    },
    expandedOnePlan: function () {
      return this.state.expanded_plan == "one";
    },
    expandedTeamPlan: function () {
      return this.state.expanded_plan == "team";
    },
    expandedCompanyPlan: function () {
      return this.state.expanded_plan == "company";
    },
    expandedEnterprisePlan: function () {
      return this.state.expanded_plan == "enterprise";
    },
    useSEK: function () {
      return localization.code == "sv";
    },
    useEURO: function () {
      return !this.useSEK();
    },
    sendAskForContactRequest: function () {
      if (new EmailValidation().validateData(this.state.email)) {
        new Submit({
          url: "/contactsales",
          fstname: this.state.fstname,
          sndname: this.state.sndname,
          email: this.state.email,
          message: this.state.message,
          plan: this.state.expanded_plan,
          method: "POST",
          ajaxsuccess: function () {
            window.location.reload();
          },
          ajaxerror: function () {
            new FlashMessage({type: "error", content: "Failed"});
          }
        }).sendAjax();
      } else {
        new FlashMessage({type: "error", content: localization.account.accountDetails.invalidEmail});
      }
    },
    render: function () {
      var self = this;
      return (
        <div>
          <div className="plan-selection-headers">
            <div
              className={classNames("plan-selection-header left", {active: this.oneYearMode()})}
              onClick={this.switchToOneYearMode}
            >
              <span className="text">
                {localization.payments.payYearly}
              </span>
            </div>
            <div
              className={classNames("plan-selection-header right", {active: this.twoYearsMode()})}
              onClick={this.switchToTwoYearsMode}
            >
              <span className="text">
                {localization.payments.payBiYearly}
              </span>
            </div>
          </div>
          <table className="plan-options-table">
            <tbody>
                <tr className="plan-options-tr">


                  <td
                    className={classNames({
                      "plan-option-td": true,
                      "hidden": this.expandedOnAnything() && !this.expandedOnePlan()
                    })}
                  >
                    <div className="plan-option">
                      <div className="plan-option-header">
                        {localization.payments.plans.one.name}
                      </div>
                      <div className="plan-option-price">
                        { /* if */ this.useSEK() &&
                          <span>
                            <span className="price">
                              {this.oneYearMode() ?
                                localization.payments.plans.one.price.SEK :
                                localization.payments.plans.one.priceWithDiscont.SEK
                              }
                            </span>
                            <span className="unit">
                              {localization.payments.priceUnit.SEK}
                            </span>
                          </span>
                        }
                        { /* if */ this.useEURO() &&
                          <span>
                            <span className="price">
                              {this.oneYearMode() ?
                                localization.payments.plans.one.price.EUR :
                                localization.payments.plans.one.priceWithDiscont.EUR
                              }
                            </span>
                            <span className="unit">
                              {localization.payments.priceUnit.EUR}
                            </span>
                          </span>
                        }
                      </div>
                      <div className="plan-option-descriptions">
                        <div className="plan-option-description-item">
                          <HtmlTextWithSubstitution secureText={localization.payments.plans.one.users} />
                        </div>
                      </div>
                      <div className="plan-option-features">
                        <div className="plan-option-feature-item">
                          {localization.payments.plans.one.support}
                        </div>
                        <div className="plan-option-feature-item">
                          {localization.payments.allFeaturesIncluded}
                        </div>
                      </div>
                      <div className="plan-option-action">
                        { /* if */  !this.expandedOnePlan() &&
                          <Button
                            size="big"
                            text={localization.payments.contact}
                            type="action"
                            onClick={this.expandOnOnePlan}
                          />
                        }
                        { /* else */ this.expandedOnePlan() &&
                          <Button
                            size="big"
                            text={localization.cancel}
                            onClick={this.expandOnNothing}
                          />
                        }
                      </div>
                    </div>
                  </td>


                  <td
                    className={classNames({
                      "plan-option-td": true,
                      "hidden": this.expandedOnAnything() && !this.expandedTeamPlan()
                    })}
                  >
                    <div className="plan-option active">
                      <div className="plan-option-header">
                        {localization.payments.plans.team.name}
                      </div>
                      <div className="plan-option-price">
                        { /* if */ this.useSEK() &&
                          <span>
                            <span className="price">
                              {this.oneYearMode() ?
                                localization.payments.plans.team.price.SEK :
                                localization.payments.plans.team.priceWithDiscont.SEK
                              }
                            </span>
                            <span className="unit">
                              {localization.payments.priceUnit.SEK}
                            </span>
                          </span>
                        }
                        { /* if */ this.useEURO() &&
                          <span>
                            <span className="price">
                              {this.oneYearMode() ?
                                localization.payments.plans.team.price.EUR :
                                localization.payments.plans.team.priceWithDiscont.EUR
                              }
                            </span>
                            <span className="unit">
                              {localization.payments.priceUnit.EUR}
                            </span>
                          </span>
                        }
                      </div>
                      <div className="plan-option-descriptions">
                        <div className="plan-option-description-item">
                          <HtmlTextWithSubstitution secureText={localization.payments.plans.team.users}/>
                        </div>
                        <div className="plan-option-description-item">
                          <HtmlTextWithSubstitution secureText={localization.payments.plans.team.stores}/>
                        </div>
                        <div className="plan-option-description-item">
                          <HtmlTextWithSubstitution secureText={localization.payments.plans.team.api} />
                        </div>
                      </div>
                      <div className="plan-option-features">
                        <div className="plan-option-feature-item">
                          {localization.payments.plans.team.support}
                        </div>
                        <div className="plan-option-feature-item">
                          {localization.payments.allFeaturesIncluded}
                        </div>
                        <div className="plan-option-feature-item">
                          {localization.payments.plans.team.branding}
                        </div>
                      </div>
                      <div className="plan-option-action">
                        { /* if */  !this.expandedTeamPlan() &&
                          <Button
                            size="big"
                            text={localization.payments.contact}
                            type="action"
                            onClick={this.expandOnTeamPlan}
                          />
                        }
                        { /* else */ this.expandedTeamPlan() &&
                          <Button
                            size="big"
                            text={localization.cancel}
                            onClick={this.expandOnNothing}
                          />
                        }
                      </div>
                    </div>
                  </td>


                  <td
                    className={classNames({
                      "plan-option-td": true,
                      "hidden": this.expandedOnAnything() && !this.expandedCompanyPlan()
                    })}
                  >
                    <div className="plan-option active">
                      <div className="plan-option-header">
                        {localization.payments.plans.company.name}
                      </div>
                      <div className="plan-option-price">
                        { /* if */ this.useSEK() &&
                          <span>
                            <span className="price">
                              {this.oneYearMode() ?
                                localization.payments.plans.company.price.SEK :
                                localization.payments.plans.company.priceWithDiscont.SEK
                              }
                            </span>
                            <span className="unit">
                              {localization.payments.priceUnit.SEK}
                            </span>
                          </span>
                        }
                        { /* if */ this.useEURO() &&
                          <span>
                            <span className="price">
                              {this.oneYearMode() ?
                                localization.payments.plans.company.price.EUR :
                                localization.payments.plans.company.priceWithDiscont.EUR
                              }
                            </span>
                            <span className="unit">
                              {localization.payments.priceUnit.EUR}
                            </span>
                          </span>
                        }
                      </div>
                      <div className="plan-option-descriptions">
                        <div className="plan-option-description-item">
                          <HtmlTextWithSubstitution secureText={localization.payments.plans.company.users} />
                        </div>
                        <div className="plan-option-description-item">
                          <HtmlTextWithSubstitution secureText={localization.payments.plans.company.stores} />
                        </div>
                        <div className="plan-option-description-item">
                          <HtmlTextWithSubstitution secureText={localization.payments.plans.company.api} />
                        </div>
                      </div>
                      <div className="plan-option-features">
                        <div className="plan-option-feature-item">
                          {localization.payments.plans.company.support}
                        </div>
                        <div className="plan-option-feature-item">
                          {localization.payments.allFeaturesIncluded}
                        </div>
                        <div className="plan-option-feature-item">
                          {localization.payments.plans.company.branding}
                        </div>
                        <div className="plan-option-feature-item">
                          {localization.payments.plans.company.sla}
                        </div>
                      </div>
                      <div className="plan-option-action">
                        { /* if */  !this.expandedCompanyPlan() &&
                          <Button
                            size="big"
                            text={localization.payments.contact}
                            type="action"
                            onClick={this.expandOnCompanyPlan}
                          />
                        }
                        { /* else */ this.expandedCompanyPlan() &&
                          <Button
                            size="big"
                            text={localization.cancel}
                            onClick={this.expandOnNothing}
                          />
                        }
                      </div>
                    </div>
                  </td>

                  <td
                    className={classNames({
                      "plan-option-td": true,
                      "hidden": this.expandedOnAnything() && !this.expandedEnterprisePlan()
                    })}
                  >
                    <div className="plan-option active">
                      <div className="plan-option-header">
                        {localization.payments.plans.enterprise.name}
                      </div>
                      <div className="plan-option-price">
                        { /* if */ this.useSEK() &&
                          <span>
                            <span className="price">
                              {this.oneYearMode() ?
                                localization.payments.plans.enterprise.price.SEK :
                                localization.payments.plans.enterprise.priceWithDiscont.SEK
                              }
                            </span>
                            <span className="unit">
                              {localization.payments.priceUnit.SEK}
                            </span>
                          </span>
                        }
                        { /* if */ this.useEURO() &&
                          <span>
                            <span className="price">
                              {this.oneYearMode() ?
                                localization.payments.plans.enterprise.price.EUR :
                                localization.payments.plans.enterprise.priceWithDiscont.EUR
                              }
                            </span>
                            <span className="unit">
                              {localization.payments.priceUnit.EUR}
                            </span>
                          </span>
                        }
                      </div>
                      <div className="plan-option-descriptions">
                        <div className="plan-option-description-item">
                          <HtmlTextWithSubstitution secureText={localization.payments.plans.enterprise.users} />
                        </div>
                        <div className="plan-option-description-item">
                          <HtmlTextWithSubstitution secureText={localization.payments.plans.enterprise.stores} />
                        </div>
                        <div className="plan-option-description-item">
                          <HtmlTextWithSubstitution secureText={localization.payments.plans.enterprise.api} />
                        </div>
                      </div>
                      <div className="plan-option-features">
                        <div className="plan-option-feature-item">
                          {localization.payments.plans.enterprise.support}
                        </div>
                        <div className="plan-option-feature-item">
                          {localization.payments.allFeaturesIncluded}
                        </div>
                        <div className="plan-option-feature-item">
                          {localization.payments.plans.enterprise.branding}
                        </div>
                        <div className="plan-option-feature-item">
                          {localization.payments.plans.enterprise.sla}
                        </div>
                        <div className="plan-option-feature-item">
                          {localization.payments.plans.enterprise.p2es}
                        </div>
                        <div className="plan-option-feature-item">
                          {localization.payments.plans.enterprise.whiteLabel}
                        </div>
                      </div>
                      <div className="plan-option-action">
                        { /* if */  !this.expandedEnterprisePlan() &&
                          <Button
                            size="big"
                            text={localization.payments.contact}
                            type="action"
                            onClick={this.expandOnEnterprisePlan}
                          />
                        }
                        { /* else */ this.expandedEnterprisePlan() &&
                          <Button
                            size="big"
                            text={localization.cancel}
                            onClick={this.expandOnNothing}
                          />
                        }
                      </div>
                    </div>
                  </td>
                  <td
                    className={classNames({
                      "plan-contact-td": true,
                      "hidden": !this.expandedOnAnything()
                    })}
                  >
                    <div className="plan-contact">
                      <InfoTextInput
                        className="plan-contact-input"
                        infotext={localization.fstname}
                        value={this.state.fstname}
                        onChange={function (v) {
                          self.setState({"fstname": v});
                        }}
                      />
                      <br/>
                      <InfoTextInput
                        className="plan-contact-input"
                        infotext={localization.sndname}
                        value={this.state.sndname}
                        onChange={function (v) {
                          self.setState({"sndname": v});
                        }}
                      />
                      <br/>
                      <InfoTextInput
                        className="plan-contact-input"
                        infotext={localization.email}
                        value={this.state.email}
                        disabled={this.props.email != ""}
                        readonly={this.props.email != ""}
                        onChange={function (v) {
                          self.setState({"email": v});
                        }}
                      />
                      <br/>
                      <textarea
                        className="plan-contact-input textarea"
                        placeholder={localization.payments.placeholder}
                        valueLink={this.linkState("message")}
                      />
                      <br/>
                      <Button
                        size="big"
                        text={localization.payments.sendmsg}
                        type="action"
                        width={344}
                        onClick={this.sendAskForContactRequest}
                      />
                    </div>
                  </td>
                </tr>
            </tbody>
          </table>
          <div className="clearfix"/>
          <div className="tax-info">
            {localization.payments.vat}
          </div>
        </div>
      );
    }
});
