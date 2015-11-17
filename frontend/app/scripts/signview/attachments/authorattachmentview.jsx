define(["legacy_code", "Underscore", "Backbone", "React", "common/backbone_mixin",
        "signview/tasks/task_mixin", "common/button", "common/checkbox",
        "signview/attachments/authorattachmentview"],
  function (legacy_code, _, Backbone, React, BackboneMixin,
            TaskMixin, Button, Checkbox, AuthorAttachmentView) {

  return React.createClass({
    mixins: [BackboneMixin.BackboneMixin, TaskMixin],

    getBackboneModels: function () {
      return [this.props.model];
    },

    propTypes: {
      model: React.PropTypes.instanceOf(AuthorAttachment).isRequired,
      canStartFetching: React.PropTypes.bool.isRequired
    },

    getInitialState: function () {
      return {showPages: false};
    },

    createTasks: function () {
      var self = this;

      if (self.props.model.isRequired() && self.props.canSign) {
        return [new PageTask({
          type: "author-attachment",
          isComplete: function () {
            return self.props.model.isAccepted();
          },
          onArrowClick: function () {
            self.props.model.setAccepted(true);
          },
          tipSide: "left",
          el: $(self.refs.checkbox.getDOMNode()),
          margin: 5
        })];
      } else {
        return [];
      }
    },

    componentDidUpdate: function (prevProps, prevState) {
      if (this.state.showPages !== prevState.showPages) {
        this.forceUpdateTasks();
      }
    },

    componentDidMount: function () {
      this.tryToStartFetchingAttachmntFilePages();
    },

    componentWillUpdate: function () {
      this.tryToStartFetchingAttachmntFilePages();
    },

    tryToStartFetchingAttachmntFilePages: function () {
      if (this.props.model.pages() == undefined && this.props.canStartFetching && !this.fetchingStarted) {
        this.fetchingStarted = true;
        this.props.model.fetch({processData: true, cache: false});
      }
    },

    render: function () {
      var self = this;
      var model = this.props.model;
      var canSign = this.props.canSign;
      var showPages = this.state.showPages;
      var isPagesLoaded = model.pages() !== undefined;

      var buttonClass = React.addons.classSet({
        "for-signing": canSign
      });

      return (
        <span>
          <div className="section author-attachment">
            <div className="col-xs-7 left">
              <table>
                <tbody>
                  <tr>
                    <td rowSpan="2"><img className="paperclip" src="/img/paperclip.png" /></td>
                    <td><h1>{model.name()}</h1></td>
                  </tr>
                  <tr>
                    <td>
                      { /* if */ model.isRequired() && canSign && !showPages &&
                        <Checkbox
                          ref="checkbox"
                          className="large-checkbox branded-checkbox"
                          checked={model.isAccepted()}
                          label={localization.docsignview.authorAttachmentsUnderstoodContent}
                          onChange={function (v) { model.setAccepted(v);}}
                        />
                      }
                    </td>
                  </tr>
                </tbody>
              </table>
            </div>
            <div className="col-xs-5 right">
              <div className="button-group small-buttons">
                <Button
                  className={buttonClass}
                  text={showPages ? localization.signviewHide : localization.reviewPDF}
                  onClick={function () {
                    self.setState({showPages: !showPages});
                  }}
                />
              </div>
            </div>
          </div>
          { /* if */ isPagesLoaded &&
            <div className="section document-pages" style={{display: showPages ? "block" : "none"}}>
              {_.map(_.range(model.pages()), function (p) {
                return (
                  <div key={p + 1} className="pagediv">
                    <img src={model.pageUrl(p + 1)}/>
                  </div>
                );
              })}
            </div>
          }
          { /* else */ !isPagesLoaded &&
            <div className="document-pages" style={{display: showPages ? "block" : "none"}}>
              <div className="waiting4page" />
            </div>
          }
          {/* if */ showPages &&
            <div className="section">
              <div className="positioned">
                <div className="col-xs-7 full-height">
                  <div className="vertical">
                    <div className="middle">
                      { /* if */ model.isRequired() && canSign &&
                        <Checkbox
                          ref="checkbox"
                          className="large-checkbox branded-checkbox"
                          checked={model.isAccepted()}
                          label={localization.docsignview.authorAttachmentsUnderstoodContent}
                          onChange={function (v) { model.setAccepted(v);}}
                        />
                      }
                    </div>
                  </div>
                </div>
                <div className="col-xs-5 pull-right right">
                  <div className="button-group small-buttons">
                    <Button
                      className={buttonClass}
                      text={localization.signviewHide}
                      onClick={function () {
                        self.setState({showPages: !showPages});
                      }}
                    />
                  </div>
                </div>
                <div className="clearfix" />
              </div>
            </div>
          }
        </span>
      );
    }
  });
});
