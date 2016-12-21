"use strict";


exports.setImageData = function(imgData) {
    return function(array) {
	var data = imgData.data;
	for (var i = 0; i < array.length; i++)
	    data[i] = array[i]
	return function (){return imgData};
    }
}

exports.canvasBoundingRect = function(canvas) {
    var rect = canvas.getBoundingClientRect();
    var o = {};
    o.x = rect.left;
    o.y = rect.top;
    o.w = rect.right-rect.left;
    o.h = rect.bottom-rect.top;
    return function(){return o};
}
