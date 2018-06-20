"use strict";

window.GettoStorage = function(storageKey,path){
  var parse = function() {
    return JSON.parse(localStorage.getItem(storageKey));
  };
  var save = function(current) {
    localStorage.setItem(storageKey, JSON.stringify(current));
  };
  var update = function(data,key,updater) {
    if(!data) {
      data = {};
    }
    data[key] = updater(data[key] || {});
    return data;
  };

  var load = function() {
    var storage = parse();
    var result = {
      global: {
        credential: {},
        menu: {},
        terminal: {}
      },
      page: []
    };
    if(storage) {
      if(storage.global) {
        result.global = Object.keys(result.global).reduce(function(acc,key) {
          acc[key] = storage.global[key] || {};
          return acc;
        }, result.global);
      }

      if(storage.page) {
        var page = storage.page[path] || {};
        result.page = Object.keys(page).reduce(function(acc,key) {
          acc.push([key, page[key]]);
          return acc;
        }, []);
      }
    }
    return result;
  };

  return {
    load: load(),

    saveGlobal: function(key,data) {
      save(update(parse(), "global", function(part) {
        return update(part, key, function(){ return data; });
      }));
    },
    savePage: function(key,data) {
      save(update(parse(), "page", function(page) {
        return update(page, path, function(part){
          return update(part, key, function(){ return data; });
        });
      }));
    }
  };
};
