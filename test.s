// this is a comment

/* so is this.
 * a multi line comment
 * in fact.
 * /* nested are also ok. */
  */

fn double(x: i64) -> i64 {
	return x * 2;
}

fn recursive(a: i64, b: i64) -> i64 {
	if a {
		return recursive(a - 1, b + 3);
	} else {
		return b;
	}
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

	x :i64 = 42;
	print(x + 4);
	print(46);
	x = 12;
	print(x + 3);
	print(15);
	y :i64 = 8;
	print(x + y);
	print(20);

	{
		z :i64 = 10;
		print(z + y + 4);
		print(22);
	}

	/* this should fail. which it currently does */
	//z + x;

}

