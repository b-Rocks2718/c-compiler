int next_collatz(int x){
  if (x & 1){
    x = 3 * x + 1;
  } else {
    x >>= 1;
  }
  return x;
}

int max(int a, int b){
  if (a > b){
    return a;
  } else {
    return b;
  }
}

int main(void) {
  int x = 121;
  int max_val = x;
  while (x != 1){
    x = next_collatz(x);
    max_val = max(max_val, x);
  }
  return max_val;
}