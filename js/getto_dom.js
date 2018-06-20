"use strict";

window.GettoDom = function(){
  return {
    focusTo: function(base,parent,focus) {
      var getParentElement = function(element,selector) {
        if(!element) {
          return null;
        }
        if(element.matches(selector)) {
          return element;
        }
        return getParentElement(element.parentElement,selector);
      };

      if(document.querySelector) {
        var baseElement = document.querySelector(base);
        if(baseElement) {
          var parentElement = getParentElement(baseElement.parentElement,parent);
          if(parentElement) {
            setTimeout(function(){
              var focusElement = parentElement.querySelector(focus);
              if(focusElement) {
                focusElement.focus();
              }
            });
          }
        }
      }
    }
  };
};
