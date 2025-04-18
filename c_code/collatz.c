unsigned ZERO_CHAR = 48;

unsigned putchar (unsigned c);

unsigned print_unisgned(unsigned x){
  unsigned d = x % 10;
  x = x / 10;
  if (x != 0){
    print_unisgned(x);
  }
  putchar(ZERO_CHAR + d);
}

unsigned collatz(unsigned x){
  if (x & 1){
    // if x is odd, return 3 * x + 1
    return x + x + x + 1;
  } else {
    // if x is even, return x/2
    return x >> 1;
  }
}

int main(void) {
  unsigned x = 121;
  unsigned max = x;
  unsigned i = 0;

  putchar(67); // C
  putchar(111); // o
  putchar(108); // l
  putchar(108); // l
  putchar(97); // a
  putchar(116); // t
  putchar(122); // z
  putchar(58); // :
  putchar(10); // \n
  

  while (x != 1){
    print_unisgned(x);
    putchar(44); // ,
    x = collatz(x);
    if (x > max) max = x;
    ++i;
  }
  print_unisgned(x);
  putchar(10); //\n
  putchar(10); //\n
  putchar(77); // M
  putchar(97); // a
  putchar(120); // x
  putchar(58); // :
  putchar(32); // space
  print_unisgned(max);
}