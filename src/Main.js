"use strict";


exports.setImageData = function(imgData) {
    return function(array) {
	return function() {
	    var data = imgData.data;
	    for (var i = 0; i < array.length; i++)
		data[i] = array[i]
	    return imgData;
	}
    }
}

exports.canvasBoundingRect = function(canvas) {
    return function() {
	var rect = canvas.getBoundingClientRect();
	var o = {};
	o.x = rect.left;
	o.y = rect.top;
	o.w = rect.right-rect.left;
	o.h = rect.bottom-rect.top;
	return o;
    }
}
