/** @jsx React.DOM */


define(['React', 'Backbone', 'tinyMCE', 'tinyMCE_theme', 'tinyMCE_noneeditable', 'tinyMCE_paste', 'legacy_code'], function(React, Backbone, tinyMCE) {


return React.createClass({
    propTypes: {
      onChange: React.PropTypes.func.isRequired,
      onPreview: React.PropTypes.func.isRequired,
      editable : React.PropTypes.bool,
      width : React.PropTypes.int,
      customtext: React.PropTypes.string,
      label : React.PropTypes.string.isRequired,
      placeholder: React.PropTypes.string.isRequired,
      disabledPlaceholder : React.PropTypes.string,
      previewLabel : React.PropTypes.string.isRequired
    },
    getInitialState: function() {
    },
    componentDidMount : function() {
      var self = this;
      setTimeout(function() {self.setupTinyMCE();},1);
    },
    setupTinyMCE: function() {
      var self = this;
      if (!this.isMounted()) return;

      // cleanup before setting up a new tinymce, else init() will do nothing.
      if (tinyMCE.editors[self.props.id]) {
        tinyMCE.remove('#' + self.props.id);
      }

      tinyMCE.baseURL = '/libs/tiny_mce';
      tinyMCE.init({
        selector: '#' + self.props.id,
        width: 275, // TODO this is overwritten by cwidth (?)
        height: 66,
        min_height: 66,
        menubar: false,
        plugins: "noneditable,paste",
        readonly: !self.props.editable,
        valid_elements: "br,em,li,ol,p,span[style<_text-decoration: underline;_text-decoration: line-through;],strong,ul,i[style<_color: #AAAAAA;]",
        width: this.props.width, // automatically adjust for different swed/eng text
        font_formats : "Verdana=Source Sans Pro, Helvetica Neue;", // fake using source sans pro / helvetica neue
        setup: function(editor) {
          editor.on('init', function() {
            $(editor.getDoc()).blur(function() {
                var text = editor.getContent();
                if ($(text).text() == self.props.placeholder)
                  text = "";
                self.props.onChange(text);
            });
            if (!self.props.editable) {
                    editor.getWin().document.body.style.color = '#AAAAAA';
            }
          });

          editor.on('PreInit', function() {
            $(editor.getContainer()).find('div[role=toolbar]').hide();
            $(editor.getContainer()).find('.mce-path').parents('.mce-panel').first().hide();
          });

          editor.on('change', function () {
            var text = editor.getContent();
            if ($(text).text() == self.props.placeholder) {
              text = "";
            }
            self.props.onChange(text);
          });


          /* Imitate a HTML5 placeholder on the TinyMCE textarea */
          var placeholder = $('#' + editor.id).attr('placeholder');
          if (typeof placeholder !== 'undefined' && placeholder !== false) {
            var is_default = false;
            editor.on('init', function() {
                // get the current content
                var cont = editor.getContent();

                // If its empty and we have a placeholder set the value
                if (cont.length === 0) {
                editor.setContent(placeholder);
                }

                var $message = $(editor.getWin().document).find("p");
                // change placeholder text color, if it's the 'placeholder text'
                if($message.text() == self.props.placeholder) {
                  $message.css("color", "#999999");
                }
            });
            editor.on('focus', function() {
                  // replace the default content on focus if the
                  // same as original placeholder
                  var $message = $(editor.getWin().document).find("p");
                if ($message.text() == self.props.placeholder) {
                    editor.setContent('');
                }
            });
            editor.on('blur', function(ed, e) {
                // if the input field is empty when leaving it, set default
                // placeholder message
                var message = editor.getContent();
                if(message == '') {
                  editor.setContent(placeholder);
                  var message = $(editor.getWin().document).find("p");
                  $(message[0]).css("color", "#999999");
                }
            });
          }
          /* END Imitate placeholder */
        }
      });
    },
    handlePreview : function() {
      this.props.onPreview();
    },
    render: function() {
      var self = this;
      var value = this.props.customtext;
      if (!this.props.editable)
        value = "<i>" + this.props.disabledPlaceholder + "</i>";
      return (
        <div className="custommessageeditor">
          <div className='label'>{this.props.label}</div>
          <a   className='preview float-right clickable' onClick={this.handlePreview}>{this.props.previewLabel}</a>
          <div className='clearfix'/>
          <div className={"custommessageeditboxwrapper "+ (!this.props.editable ? "disabled" : "")}>
            <textarea id={this.props.id}
                      className="editor"
                      placeholder={this.props.placeholder}
                      value={value}
                      style={{display: "none"}}
                      disabled={this.props.editable? undefined : "YES"}
            >
            </textarea>
        </div>
       </div>
      );
    }
  });

});
