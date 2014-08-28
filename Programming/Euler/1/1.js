function threeFive(n) {
	var sum = 0;
	var i = 0;
	for(i ; i < n; i++) {
		if(i%3 === 0) {
			sum += i;
		} else if(i%5 === 0) {
			sum += i;
		};
	};
	console.log(sum);
};
