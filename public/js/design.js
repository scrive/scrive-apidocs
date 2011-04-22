/*!
 * Design View
 * Rewritten by Eric Normand
 * April 2011
 */
(function(window, undefined) {
  // yay javascript!
  // add in default implementations of methods for getters and setters
  // should be added to individual prototype objects (one for each "class")
  var addGettersAndSetters = function(obj, names) {
    for(var i = 0, var l = names.length; i < l; i++) {
      var name = names[i];
      obj["set" + name] = function(v) {
        return this.set(name, v);
      };
      obj["get" + name] = function() {
        return this.get(name);
      };
    }
  };

  // the basic prototype for a simple object system
  // getting returns a value or undefined
  // setting sets the value, runs the view update function (if it exists),
  //   and returns this (for chaining)
  var modelPrototype = {
    get: function(name) {
      if(this.state) {
        return this.state[name];
      } else {
        return undefined;
      }
    },
    set: function(name, value) {
      if(!this.state) {
        this.state = {};
      }
      this.state[name] = value;
      if(this["update" + name]){
        this["update" + name]();
      }
      return this;
    }
  };

  var placementPrototype = {
    parent: modelPrototype,
    updatePage: function() {
      this.reattach();
    },
    updateX: function() {
        this.div.left(this.getX());
    },
    updateY: function() {
        this.div.top(this.getY());
    },
    updateFieldMaster: function() {
      var text = this.getFieldMaster().getValue();
      if(!text) {
        text = this.getFieldMaster().getInfoText();
      }
      this.div.text(text);
    },
    updateText: function() {
      this.updateFieldMaster();
    },
    remove: function() {
      var pls = this.getFieldMaster().getPlacements();
      var i = pls.indexOf(this);
      pls.splice(i, i);
      this.div.detach();
    },
    detach: function() {
      this.div.detach();
    },
    attach: function() {
      $("#page" + this.getPage()).append(this.div);
    }
  };

  addGettersAndSetters(placementPrototype, ["X", "Y", "Page", "FieldMaster"]);

  // create a placement
  var newPlacement = function() {
    // add div
    return { parent = placementPrototype };
  };

  var fieldPrototype = {
    parent: modelPrototype,
    getPlacements: function() {
      if(!this.placements) {
        this.placements = [];
      }
      return this.placements;
    },
    addPlacement: function() {
      var placement = newPlacement();
      placement.setFieldMaster(this);
      this.getPlacements().push(placement);
    },
    updateValue: function() {
      this.div.find("input").val(this.getValue());
      var pls = this.getPlacements();
      for(var i = 0, var l = pls.length; i < l; i++) {
        pls.updateFieldMaster();
      }
    },
    updateInfoText: function() {
      this.div.find("input").attr("infotext", this.getInfoText());
      var pls = this.getPlacements();
      for(var i = 0, var l = pls.length; i < l; i++) {
        pls.updateFieldMaster();
      }
    },
    remove: function() {
      var flds = this.getSignatoryMaster().getFields();
      var i = flds.indexOf(this);
      flds.splice(i, i);
      this.div.detach();
      var pls = this.getPlacements();
      for(var i = 0, var l = pls.length; i < l; i++) {
        pls.detach();
      }
    },
    detach: function() {
      this.div.detach();
      var pls = this.getPlacements();
      for(var i = 0, var l = pls.length; i < l; i++) {
        pls.detach();
      }
    },
    attach: function() {
      this.getSignatoryMaster().div.find("signatories").append(this.div);
      var pls = this.getPlacements();
      for(var i = 0, var l = pls.length; i < l; i++) {
        pls.attach();
      }
    },
    updateSignatoryMaster: function() {
      this.getSignatoryMaster().div.find("signatories").append(this.div);
    }
  };

  addGettersAndSetters(fieldPrototype, ["Value", "InfoText", "Status", "Type", "SignatoryMaster"]);

  var createField = function() {
    return { parent: fieldPrototype };
  };

  var signatoryPrototype = {
    parent = modelPrototype
  };

  addGettersAndSetters(signatoryPrototype, ["Title"]);

  var createSignatory = function() {
    return { parent: signatoryPrototype };
  };

  // State of the document //
  
  var author  = createSignatory();
  var parties = [];

  var authorSignsLast = false;

  // operations //

  /**
     Add a party
  **/
  function addSignatory() {
    var sig = {};
    parties.push(sig);
    // begin view here

    // end   view update
  }

  /**
     Add a field to a signatory
   **/

  /**
     Store everything in readily accessible form.
   **/
})(window);

// I want to be able to just redraw the part that has changed.
// I don't want to optimize prematurely.
