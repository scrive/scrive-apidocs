
define(["legacy_code", "Underscore", "Backbone", "React", "common/backbone_mixin", "signview/fileview/fileview"],
  function (legacy_code, _, Backbone, React, BackboneMixin, FileView) {

  return function (args) {
    this.model = args.file;

    var el = $("<div>")[0];
    this.view = React.render(
      <FileView model={this.model} signview={args.signview} arrow={args.arrow} />,
    el);
    this.view.el = el;

    return this;
  };
});
