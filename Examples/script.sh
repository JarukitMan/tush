#!/home/edward/Documents/my-codes/tush/tush
#Doesn't work yet, obviously.

fastfetch
echo "That's Crazy."
1 + 2 - 3 * 4 + 5 #implicit echo

for i in [0..100] 
{
	while i < 1000 {
		echo -n i #we can infer that this is referring to the variable not the literal character 'i'
		       #because it's 1. defined 2. alone 3. not inside a string.
		i = i + 1
	} else {
		echo "done"
	}
}

echo "Something in the middle lol"

else {
	echo done for real #we know this is not referring to a variable because we know
	                   #only variables and literals can be passed to outsider functions
			   #and this sentence is not a variable. It even contains space.
}

#exit
