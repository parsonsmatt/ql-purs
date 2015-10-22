// module Util

exports.replaceLocation = function(url) {
    return function() {
        window.location.replace(url);
    }
}
