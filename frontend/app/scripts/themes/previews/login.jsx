/** @jsx React.DOM */

define(["React","common/backbone_mixin","admin/brandeddomain/domainviewmodel","themes/theme"  ,"legacy_code","common/button","common/uploadimagebutton","common/select","common/infotextinput"], function(React, BackboneMixin, DomainViewModel,Theme,_Legacy, Button, UploadImageButton,NewSelect,InfoTextInput) {

return React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.model];
    },
    render: function() {
      var self = this;
      var model = this.props.model;

      return (
        <div className="login-preview" style={{"background-color": model.brandColor()}}>
          <div className="logo-wrapper">
            <img src={ model.logo() || "/img/logo_email.png"}/>
            <div className="divider-line" />

            <p className="small" style={{"color": model.brandTextColor()}}>E-signing powered by Scrive</p>
          </div>

          <div className="content-container" style={{"font-family": model.font()}}>
            <div className="content">
              <InfoTextInput infotext="Email address" style={{"width":"200px"}} />
              <InfoTextInput infotext="Password" style={{"width": "200px"}} buttonTitle="Forgot?" onClick={function() {}} />
              <Button type="main" text="Log in" style={{"background-color": model.brandColor(), "border-color": model.brandTextColor(), "color": model.brandTextColor()}}/>

              <p style={{"color": model.brandTextColor()}}>Don't have an account? <a style={{"color": model.brandTextColor()}} href="#">Sign up for free</a></p>

              <div className="select-container">
                <NewSelect.Select style={{"width":"150px", "background-color": model.brandColor(), "border-color": model.brandTextColor(), "color": model.brandTextColor()}} name="English" options={[]} />
              </div>
            </div>
          </div>

        </div>
      );
    }
  });
});
