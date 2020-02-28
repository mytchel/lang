// this is a comment

/* so is this.
 * a multi line comment
 * in fact.
  */

/* E  -> T E'
   E' -> + T E' | ;
   T  -> F T';
   T' -> * F T' | ;
   F  -> (E) | id

   a - b + c

   E -> T + E | T
   T -> F * T | F
   F -> (E) | id

   a - (b + c)

   what we want

   E -> E + T | T
   T -> T * F | F
   F -> (E) | id

   (a - b) + c

   the problem is that the grammer is left recursive and
   the first set of rules was to get rid of that but 
   that makes it harder later on. Or well, I don't know how
   my structure should represent it. Well I kind of do.
   It's basically a linked list with a start then an op
   then another term which is either a thing or another 
   expression...

*/

10 - 4 + 2;
15 * 4 * 2 + 84 * 4;
10 + 15 + 35;
32 + 87 * 19;
43 * 48 + 2;
2 + 4 + 2 * 4;
5 + 3;
13 * (22 + 4) * 3 + 15;

/*
15.03 * (4.234 + 9.44) + (44 * 22.34);

15 * i + (19 * hello_there) + 1;

13 - 12 / 4;
test(12, again(14, 19 * 4));

if (4 + 9) {
	12 * 44 + (9 - test(hello));
}

if 2 {
	then(this);
	4 + 21;
} else {
	2 * (11 + 9);
	this_other(thing);
}


{
	what(about(this) + 4);
	422 * 11 - 9;
}
*/
