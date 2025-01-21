
int* p;

int main(int argc, int **argv){
  int ** x = (int **)1;
  int * y = (int *)x;
  x = &y; // this line fails right now, but it shouldn't
  *y = 2;
  return 1;
}