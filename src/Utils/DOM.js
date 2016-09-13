// module Utils.DOM

exports.waitLoaded = function(cb, eb) {
    if (window.document.readyState === "complete") {
        return cb({});
    } else {
        return window.addEventListener("load", cb);
    }
};

exports.onLoad = function(action) {
    return function() {
        window.addEventListener("load", function() {
            action();
        });
    };
};

exports.blur = function(el) {
    return function() {
        return el.blur();
    };
};

exports.focus = function(el) {
    return function() {
        return el.focus();
    };
};

exports.getBoundingClientRect = function(el) {
    return function() {
        return el.getBoundingClientRect();
    };
};

exports.offsetLeft = function(el) {
    return function() {
       return el.offsetLeft;
    };
};

var getTextWidth_ = function(text) {
    return function(font) {
        var canvas = document.createElement("canvas");
        var context = canvas.getContext("2d");
        context.font = font;
        return context.measureText(text).width;
    };
};

exports.getTextWidthPure = getTextWidth_;

exports.getTextWidth = function(text) {
    return function(font) {
        return function() {
            return getTextWidth_(text)(font);
        };
    };
};

exports.elementEq = function(a) {
    return function(b) {
        return function() {
            return a == b;
        };
    };
};

exports.scrollTop = function(el) {
    return function() {
        return el.scrollTop;
    };
};

exports.scrollLeft = function(el) {
    return function() {
        return el.scrollLeft;
    };
};

exports.getOffsetClientRect = function(el) {
  return function() {
    var rect = el.getBoundingClientRect();
    return {
      top: rect.top + document.body.scrollTop,
      left: rect.left + document.body.scrollLeft,
      width: rect.width,
      height: rect.height
    };
  };
};

exports.setFontSize = function(element) {
  return function(fontSize) {
    return function() {
      return element.style.fontSize = fontSize;
    };
  };
};

exports.getOffsetWidth = function(element) {
  return function() {
    return element.offsetWidth;
  };
};

exports.getOffsetHeight = function(element) {
  return function() {
    return element.offsetHeight;
  };
};

exports.getScrollWidth = function(element) {
  return function() {
    return element.scrollWidth;
  };
};

exports.getScrollHeight = function(element) {
  return function() {
    return element.scrollHeight;
  };
};

exports.open = function(strUrl) {
  return function(strWindowName) {
    return function(strWindowFeatures) {
      return function(windowObjectReference) {
        return function() {
          windowObjectReference.open(strUrl, strWindowName, strWindowFeatures);
        };
      };
    };
  };
};

exports.close = function(windowObjectReference) {
  return function() {
    windowObjectReference.close();
  };
};




exports.centerPopupWindowFeatures = function(w) {
  return function(h) {
    return function(windowObjectReference) {
      return function() {
        var dualScreenLeft = windowObjectReference.screenLeft != undefined ? windowObjectReference.screenLeft : screen.left;
        var dualScreenTop = windowObjectReference.screenTop != undefined ? windowObjectReference.screenTop : screen.top;
        var width = windowObjectReference.innerWidth ? windowObjectReference.innerWidth : document.documentElement.clientWidth ? document.documentElement.clientWidth : screen.width;
        var height = windowObjectReference.innerHeight ? windowObjectReference.innerHeight : document.documentElement.clientHeight ? document.documentElement.clientHeight : screen.height;
        var left = ((width / 2) - (w / 2)) + dualScreenLeft;
        var top = ((height / 2) - (h / 2)) + dualScreenTop;
        return 'scrollbars=yes, width=' + w + ', height=' + h + ', top=' + top + ', left=' + left;
      };
    };
  };
};
