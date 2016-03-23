// module Test.Utils

exports.nonWhite = function(fp) {
    return function(cb, eb) {
        try {
            var fs = require("fs"),
                PNG = require("pngparse");
            return PNG.parseFile(fp, function(err, id) {
                if (err) return eb(err);
                var x,
                    y,
                    idx,
                    isWhite,
                    count = 0;
                for (x = 0; x < id.width; x++) {
                    for (y = 0; y < id.height; y++) {
                        isWhite = id.getPixel(x, y) == 0xFFFFFFCC;
                        if (!isWhite) {
                            count = count + 1;
                        }
                    }
                }
                return cb({
                    count: count,
                    percent: 100 * count / (id.width * id.height)
                });
            });
        }
        catch (e) {
            return eb(e);
        }
    };

};
