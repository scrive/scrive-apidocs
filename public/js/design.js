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
    for(var i = 0; i < names.length; i++) {
      var name = names[i];
      obj["set" + name] = function(v) {
        return this.set(name, v);
      };
      obj["get" + name] = function() {
        return this.get(name);
      };
    }
  };

  var addCollectionMethods = function(obj, name, f) {
    var lname = name + "List";
    obj["get" + lname] = function() {
      var l = this.get(lname);
      if(!l) {
        l = [];
        this.set(lname, l);
      }
      return l;
    };
    obj["add" + name] = function() {
      var item = f(this);
      this.get(lname).push(item);
      var obs = this["onAdd" + name];
      if(obs) {
        obs(item);
      }
      return item;
    };
    obj["remove" + name] = function(item) {
      var l = this.get(lname);
      var i = l.indexOf(item);
      l.splice(i, i);
      var obs = this["onRemove" + name];
      if(obs) {
        obs(item);
      }
      return this;
    };
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
      this.getFieldMaster().removePlacement(this);
      this.div.detach();
      return this;
    },
    detach: function() {
      this.div.detach();
      return this;
    },
    attach: function() {
      $("#page" + this.getPage()).append(this.div);
      return this;
    }
  };

  addGettersAndSetters(placementPrototype, ["X", "Y", "Page", "FieldMaster"]);

  // create a placement
  var newPlacement = function() {
    // add div
    return { parent: placementPrototype };
  };

  // Field Definition
  var fieldPrototype = {
    parent: modelPrototype,
    updateValue: function() {
      this.div.find("input").val(this.getValue());
      var pls = this.getPlacements();
      for(var i = 0; i < pls.length; i++) {
        pls.updateFieldMaster();
      }
    },
    updateInfoText: function() {
      this.div.find("input").attr("infotext", this.getInfoText());
      var pls = this.getPlacements();
      for(var i = 0; i < pls.length; i++) {
        pls.updateFieldMaster();
      }
    },
    remove: function() {
      this.getSignatoryMaster().removeField(this);
      this.div.detach();
      var pls = this.getPlacements();
      for(var i = 0; i < pls.length; i++) {
        pls.detach();
      }
      return this;
    },
    detach: function() {
      this.div.detach();
      var pls = this.getPlacements();
      for(var i = 0; i < pls.length; i++) {
        pls.detach();
      }
      return this;
    },
    attach: function() {
      // this is not the right selector
      this.getSignatoryMaster().div.find("signatories").append(this.div);
      var pls = this.getPlacements();
      for(var i = 0; i < pls.length; i++) {
        pls.attach();
      }
      return this;
    },
    updateSignatoryMaster: function() {
      this.getSignatoryMaster().div.find("signatories").append(this.div);
    }
  };

  addGettersAndSetters(fieldPrototype, ["Value", "InfoText", "Status", "Type", "SignatoryMaster"]);
  addCollectionMethods(fieldPrototype,
                       "Placement",
                       function(field) { return newPlacement().setFieldMaster(field); });


  var newField = function() {
    var ret = { parent: fieldPrototype };
    ret.div = $('<div class="field"></div>');
    return ret;
  };

  var signatoryPrototype = {
    parent: modelPrototype
  };

  addGettersAndSetters(signatoryPrototype, ["Title", "Secretary", "Signatory", "State"]);
  addCollectionMethods(signatoryPrototype,
                       "Field",
                       function(sig) {
                         return newField().setSignatoryMaster(sig);
                       });

  var newSignatory = function() {
    var ret = { parent: signatoryPrototype };
    ret.div = $("");
    return ret;
  };

  var statePrototype = {
    parent: modelPrototype,
    set2Mode: function() {
      setMode("2 Mode");
      return this;
    },
    setListMode: function() {
      setMode("List Mode");
      return this;
    },
    setStep2: function() {
      setStep(2);
    },
    setStep3: function() {
      setStep(3);
    },
    updateMode: function() {
      if(this.getMode() === "2 Mode") {

      } else if(this.getMode() === "List Mode") {

      } else {
        console.log("What mode is this? " + this.getMode());
      }
    },
    updateStep: function() {
      if(this.getStep() === 2) {

      } else if(this.getStep() === 3) {

      } else {
        console.log("What step is this? " + this.getStep());
      }
    }
  };

  addGettersAndSetters(statePrototype, ["Author", "Mode", "Step", "CurrentPartner"]);
  addCollectionMethods(statePrototype,
                       "Partner",
                       function(state) {
                         return newSignatory().setState(state);
                       });

  var newState = function() {
    var ret = { parent: statePrototype };
    ret.div = $(".signatories");
    return ret;
  };

  // State of the document //

  var state = newState();

  /**
     Store everything in readily accessible form.
   **/
  window.design = state;
})(window);
