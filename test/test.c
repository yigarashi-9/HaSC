int a;

int f(int *b, int c){
  return 1;
}

int main(){
  int a[10];
  int *b, c;
  *(b + c) = a[1];
  b = &c;
}
