// this is a comment
int main(void) {
  /* so is this
     return 27 */
  int x = 10;
  int y = 0;
  loop: if (x == 0)
    goto done;
  y += x;
  x--;
  goto loop;
  done: return y;
}
