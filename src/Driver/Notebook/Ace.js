// module Driver.Notebook.Ace

exports.aceSetOption = function(s) {
    return function (a) {
      return function (editor) {
        return function () {
          editor.setOption(s, a);
        };
      };
    };
};
