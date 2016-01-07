/* Mixin for placement views that have typesetter
   To use this mixin, getPlacement() and getTypeSetterClass() functions should be implemented
   and prod closeAllTypeSetters should be set.
*/

define(["jquery", "Underscore", "Backbone", "React", "legacy_code"],
  function ($, _, Backbone, React) {

  return {
    propTypes: {
      closeAllTypeSetters: React.PropTypes.func.isRequired
    },

    getInitialState: function () {
      return {hasTypeSetter: false, typeSetterDiv: undefined, typeSetterComp: undefined};
    },

    componentDidMount: function () {
      this.updateTypeSetter();
    },

    componentDidUpdate: function () {
      this.updateTypeSetter();
    },

    componentWillUnmount: function () {
      if (this.state.typeSetterDiv) {
        React.unmountComponentAtNode(this.state.typeSetterDiv[0]);
        this.state.typeSetterDiv.remove();
      }
    },

    hasTypeSetter: function () {
      return this.state.hasTypeSetter;
    },

    updateTypeSetter: function () {
      if (this.state.hasTypeSetter && !this.state.typeSetterDiv) {
        this.createTypeSetter();
      } else if (!this.state.hasTypeSetter && this.state.typeSetterDiv) {
        this.destroyTypeSetter();
      }
    },

    createTypeSetter: function () {
      var self = this;
      var typeSetterDiv = $("<div/>");
      var typeSetterComp = React.render(React.createElement(self.getTypeSetterClass(), {
        model: self.getPlacement(),
        element: $(this.getDOMNode())[0],
        hideFunc: function () {
          self.setState({hasTypeSetter: false});
        }
      }), typeSetterDiv[0]);
      $("body").append(typeSetterDiv);
      this.setState({typeSetterDiv: typeSetterDiv, typeSetterComp: typeSetterComp});

    },

    destroyTypeSetter: function () {
      if (this.state.typeSetterDiv) {
        React.unmountComponentAtNode(this.state.typeSetterDiv[0]);
        this.state.typeSetterDiv.remove();
        this.setState({typeSetterDiv: undefined, typeSetterComp: undefined});
      }
    },

    openTypeSetter: function () {
      this.setState({hasTypeSetter: true});
    },

    closeTypeSetter: function () {
      this.setState({hasTypeSetter: false});
    },

    toogleTypeSetterAndCloseOther: function () {
      if (this.state.hasTypeSetter) {
        this.closeTypeSetter();
      } else {
        this.props.closeAllTypeSetters();
        this.openTypeSetter();
      }
    },

    forceUpdateTypeSetterIfMounted: function () {
      if (this.state.typeSetterComp && this.state.typeSetterComp.isMounted()) {
        this.state.typeSetterComp.forceUpdate();
      }
    }
  };
});
