unsigned ZERO_CHAR = 48;

unsigned putchar(unsigned c);

unsigned print(unsigned* str){
  for (unsigned* i = str; *i != 0; i++){
    putchar(*i);
  }
}

unsigned hello[13] = 
  {72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100, 33, 0};

int main(void) {
  print(hello);
}