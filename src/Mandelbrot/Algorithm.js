"use strict";

exports.calculatePixel = function(zr, zi, cr, ci, n) {
    while (n > 0 && Math.sqrt(zr*zr + zi*zi) < 2.0) {
	var zr_tmp = (zr*zr - zi*zi) + cr;
	zi = (zi*zr + zr*zi) + ci;
	zr = zr_tmp;
	n--;
    }
    return n;
}
