"use strict";

window.GettoSearch = function(){
  var load = function() {
    return location.search.substring(1).split("&").reduce(function(acc,query) {
      if(query.length > 0) {
        var pair = query.split("=");
        var key = pair.shift();
        var value = decodeURIComponent(pair.join("="));
        acc.push([key, value]);
      }
      return acc;
    }, []);
  };

  return {
    load: load(),

    redirectTo: function(href) {
      location.href = href;
    },

    searchTo: function(search) {
      var args = search.reduce(function(acc,pairs){
        acc.push(pairs.join("="));
        return acc;
      },[]);
      history.pushState(null,null, "?" + args.join("&"));
    }
  };
};
