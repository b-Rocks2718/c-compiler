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

int main(void) {
  putchar(120);
  putchar(58);

  int x = -1;
  if (x > 0) {
    print_unisgned(x);
  } else {
    putchar(45); // -
    print_unisgned(-x);
  }

  putchar(10);
  putchar(121);
  putchar(58);

  unsigned y = -1;
  if (y > 0) {
    print_unisgned(y);
  } else {
    putchar(45); // -
    print_unisgned(-y);
  }
}