int f(int a, int *b){
  return *(b + a);
}

int main(){
  int *c, d;
  d = *f(4, c);
}
