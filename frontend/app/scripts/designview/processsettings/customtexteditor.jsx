/** @jsx React.DOM */


define(['React', 'legacy_code'], function(React) {


return React.createClass({
    propTypes: {
      onChange: React.PropTypes.func.isRequired,
      onPreview: React.PropTypes.func.isRequired,
      editable : React.PropTypes.bool,
      customtext: React.PropTypes.string,
      label : React.PropTypes.string.isRequired,
      placeholder: React.PropTypes.string.isRequired,
      disabledPlaceholder : React.PropTypes.string,
      previewLabel : React.PropTypes.string.isRequired
    },
    handlePreview : function() {
      this.props.onPreview();
    },
    render: function() {
      var self = this;
      var value = this.props.customtext;
      if (!this.props.editable)
        value = this.props.disabledPlaceholder;
      return (
        <div className="custommessageeditor">
          <div className='label'>{this.props.label}</div>
          <a   className='preview float-right' onClick={this.handlePreview}>{this.props.previewLabel}</a>
          <div className='clearfix'/>
          <div className="custommessageeditboxwrapper">
            <textarea
              id={self.props.id}
              className={"editor " + (!this.props.editable ? "disabled" : "")}
              placeholder={this.props.placeholder}
              value={value}
              disabled={this.props.editable? undefined : "disabled"}
              onChange={function(e) {
                if (self.props.editable) {
                  self.props.onChange(e.target.value);
                }
              }}
            >
            </textarea>
        </div>
       </div>
      );
    }
  });

});
