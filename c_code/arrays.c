unsigned ZERO_CHAR = 48;
unsigned ONE_CHAR = 49;
unsigned TWO_CHAR = 50;
unsigned THREE_CHAR = 51;

unsigned putchar (unsigned c);

unsigned print_unisgned(unsigned x){
  unsigned d = x % 10;
  x = x / 10;
  if (x != 0){
    print_unisgned(x);
  }
  putchar(ZERO_CHAR + d);
}

unsigned test[4][3][2] = {{{0}}, {{9, 34}, {1}}, {{1}, {2}}};

int main(void) {
  for (int i = 0; i < 3; ++i){
    for (int j = 0; j < 4; ++j){
      print_unisgned(test[j][i][0]);
      putchar(32);
    }
  }
}