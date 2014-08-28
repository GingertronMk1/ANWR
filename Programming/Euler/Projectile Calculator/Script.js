function trajectory(a,b) {
	var totalDistance = parseFloat((2(a^2)(Math.sin(Math.radians(b)))(Math.cos(Math.radians(b))))/9.81);
	console.log(totalDistance);
}

document.getElementById('Calculate').onclick = trajectory(document.getElementById("velocity").value, document.getElementById("angle").value)
