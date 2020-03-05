// this is a comment

/* so is this.
 * a multi line comment
 * in fact.
  */

fn double(x) {
	/* need to add return but for now
	   all expressions return and the last
	   one is used as the return for a block.
	   This is wasting temporary variables though
	   as you can see if you look through the output
	   where each print is returning something which
	   is being saved and the temp counter is incremented.
	   */
	x * 2;
}

/* this doesn't work yet 
   need to make if and else labels
   global.
*/

fn recursive(a, b) {
	x := 2;
	if a {
		x = recursive(a - 1, b + 3);
	} else {
		x = b;
	}

	x;
}

fn main() {
	print(10 - 4 + 2);
	print(8);
	print(1 + 2 + double(5) - 4);
	print(9);

	print(recursive(4, 1));
	print(13);

	print(10 - 4 + 2);
	print(8);
	print(3 * 3 + 2);
	print(11);
	print(3 + 3 * 3);
	print(12);
	print(10 * (4 - 2));
	print(20);

	if (4 + 9) {
		print(1);
	}
	print(1);

	if 10 - 10 {
		print(1);
	} else {
		print(0);
	}

	print(0);

	if 1 {
		print(1);
	} else {
		print(0);
	}

	print(1);

	x := 42;
	print(x + 4);
	print(46);
	x = 12;
	print(x + 3);
	print(15);
	y := 8;
	print(x + y);
	print(20);

	{
		z := 10;
		print(z + y + 4);
		print(22);
	}

	/* this should fail. which it currently does */
	//z + x;
}

