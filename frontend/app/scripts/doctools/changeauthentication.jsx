/** @jsx React.DOM */

define(['React','common/backbone_mixin','Backbone','common/select', 'legacy_code'], function(React, BackboneMixin, Backbone, Select) {
    return React.createClass({
        mixins : [BackboneMixin.BackboneMixin],
        getBackboneModels : function() {
          return [this.props.model];
        },
        propTypes : {
          model : React.PropTypes.object
        },
        authenticationMethodName : function() {
          var method = model.newAuthenticationMethod();
          if(method == 'standard') {
              return localization.docview.signatory.authenticationStandard;
          }
          else if(method == 'sms_pin') {
              return localization.docview.signatory.authenticationSMSPin;
          }
          else if(method == 'eleg') {
              return localization.docview.signatory.authenticationELeg;
          }
        },
        setAuthenticationMethod : function(v) {
          model.setNewAuthenticationMethod(v);
          return true;
        },
        setAuthenticationValue : function(event) {
          model.setNewAuthenticationValueInvalid(false);
          model.setNewAuthenticationValue(event.target.value);
        },
        render : function() {
            var model = this.props.model;
            var signatory = model.signatory();
            var SelectComponent =  Select.Select;
            var selectLabel = $('<div>' + localization.docview.changeAuthentication.methodLabel + '</div>')
            $('.put-person-name',selectLabel).html($('<strong>').text(signatory.smartname()));
            return (
                <div>
                    <p dangerouslySetInnerHTML={{__html: selectLabel.html()}} />
                    <SelectComponent
                             name={this.authenticationMethodName()}
                             onSelect={this.setAuthenticationMethod}
                             options={ [ { name : localization.docview.signatory.authenticationStandard,
                                           value : "standard"
                                         }
                                       , { name : localization.docview.signatory.authenticationSMSPin,
                                           value : "sms_pin"
                                         }
                                       , { name : localization.docview.signatory.authenticationELeg,
                                           value : "eleg"
                                         }
                                       ] }
                    />
                    {/*if*/ model.newAuthenticationMethod() != 'standard' &&
                    <div>
                        <p>{model.newAuthenticationMethod() == 'sms_pin' ? localization.phone : localization.docsignview.personalNumberLabel}</p>
                        <div className={model.newAuthenticationValueInvalid() ?
                                        'info-text-input obligatory-input' : 'info-text-input'}>
                            <input type='text'
                                   value={model.newAuthenticationValue()}
                                   onChange={this.setAuthenticationValue}
                            />
                        </div>
                        <p className='gray-50-percent'>{localization.docview.changeAuthentication.valueInfotext}</p>
                    </div>
                    }
                </div>
            );
        }
    });
});
