(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
  "use strict";
  
  function HighlightState(cm, options) {
    this.options = options;
    this.timeout = null;
  }
  
  function parseOptions(_cm, options) {
    if (options instanceof Function) return {getAnnotations: options};
    if (!options || options === true) options = {};
    return options;
  }
  
  function displayHighlight(cm) {
    var state = cm.state.ternHighlight;
    if (!state) return;
    var server = CodeMirror.tern.getServer(cm);
    if (!server) return;
    
    var query = {
      type : "highlight",
      text : cm.getValue()
    };
    
    var doc = {
      query : query
    };
    server.server.request(doc, function(error, response) {
      if (error) {
        updateHighlight(state, {});
      } else {
        var highlight = response;
        updateHighlight(state, highlight);
      }
    });
  }
  
  function updateHighlight(state, highlight) {
    var content = JSON.stringify(highlight, null, ' ');
    if (state.options.node.value) {
      state.options.node.value = content 
    } else {
      state.options.node.innerHTML = content;
    }
  }
  
  function onChange(cm) {
    var state = cm.state.ternHighlight;
    if (!state) return;
    clearTimeout(state.timeout);
    state.timeout = setTimeout(function(){displayHighlight(cm);}, state.options.delay || 500);
  }

  CodeMirror.defineOption("highlight", false, function(cm, val, old) {
    if (old && old != CodeMirror.Init) {
      cm.off("change", onChange);
    }
    if (val) {
      var state = cm.state.ternHighlight = new HighlightState(cm, parseOptions(cm, val));
      cm.on("change", onChange);
    }
    displayHighlight(cm);
  });
  
});
  