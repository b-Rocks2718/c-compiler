int main(void) {
  int x = 121;
  int max = x;
  while (x != 1){
    if (x % 2){
      x = 3 * x + 1;
    } else {
      x = x / 2;
    }

    if (x > max){
      max = x;
    }
  }
  return max;
}
