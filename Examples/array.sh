#!/home/edward/Documents/my-code/tush/tush

# Currently an error! The ',' was recognized and a tup of arrs is returned.
let arr = [1, 10, 100, 1000, 10000, (10 * 10000), (1000 * 1000)]
let mat = [ [0,1,2] [1 2 3] [2 3 4] ]
let tup = {{1 2 3},{'4' '5' '6'},{"seven" "eight" "nine"}}

fn (int b int c) add (int a int d) ( return a + b + c + d )

for num in arr {
	num
}

for i in [0..len mat] {
	for j in [0..len mat.i] {
		echo -n f"What is \\\{this}...{mat.i.j} "
	}
	echo
	for j in [0..len mat.i] {
		echo -n f"{mat.i.j} "
	}
}

#should print 1 10 100 1000 in different lines.
