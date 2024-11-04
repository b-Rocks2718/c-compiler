
int main(void){
  int x = 10;
  int y = 7;

  switch (x){
    case 7:
      switch (x){
        case 10:
          return 21;
      }
      return 22;
    case 10:
      switch (y){
        case 10:
          return 12;
        default:
          return -1;
      }
  }
}