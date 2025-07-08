#!/home/edward/Documents/my-code/tush/tush

# Currently an error! The ',' was recognized and a tup of arrs is returned.
let arr = [1, 10, 100, 1000, 10000, 10 * 10000, (1000 * 1000)]
let mat = [ [0,1,2] [1 2 3] [2 3 4] ]
let tup = {{1 2 3} {'4' '5' '6'} {"seven" "eight" "nine"}}

opr (Int b Int c) add (Int a Int d) = ( return a + b + c + d )

_ = for num in arr {
	return num
}

echo mat

_ = for i in 0..len mat - 1 {
	for j in 0..len mat:i - 1 {
		echo -n f"What is \\\{this}...{mat:i:j} "
	}
	echo
	for j in 0..len mat:i - 1 {
		echo -n f"{mat:i:j} "
	}
}

_ = for i in arr {
	echo i
}

#should print 1 10 100 1000 10000 100000 1000000 in different lines.
