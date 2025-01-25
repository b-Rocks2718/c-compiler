
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

int test(int a, int* x, int* y){
  *x = a * a + 10;
  *y = (*x) * a;
}

int main(void){
  int input = 2;
  int output1 = 0;
  int output2 = 0;
  test(input, &output1, &output2);
  print_unisgned(output1);
  putchar(10);
  print_unisgned(output2);
}