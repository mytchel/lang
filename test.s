// this is a comment

/* so is this.
 * a multi line comment
 * in fact.
  */
/*
fn double(x) {
	x * 2;
}
*/
fn main() {
	10 - 4 + 2;
	8;
	3 * 3 + 2;
	11;
	3 + 3 * 3;
	12;
	10 * (4 - 2);
	20;

	if (4 + 9) {
		1;
	}
	1;

	if 10 - 10 {
		1;
	} else {
		0;
	}

	0;

	if 1 {
		1;
	} else {
		0;
	}

	1;

	x := 42;
	x + 4;
	46;
	x = 12;
	x + 3;
	15;
	y := 8;
	x + y;
	20;

	{
		z := 10;
		z + y + 4;
		22;
	}

	/* this should fail. which it currently does */
	//z + x;
}

