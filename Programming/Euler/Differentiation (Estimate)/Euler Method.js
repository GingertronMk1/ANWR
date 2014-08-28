function euler(a, b, c) {
	var dydx = parseFloat(a);
	var y1 = parseFloat(b);
	var deltax = parseFloat(c);
	var deltay = dydx * deltax;
	console.log(deltay);
}
