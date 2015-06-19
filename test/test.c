int a;

int f(int *b, int c){
  return *(b+c);
}

int main(){
  int a, b;
  f(a, b);
}
