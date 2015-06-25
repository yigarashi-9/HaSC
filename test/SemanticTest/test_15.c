int f(int a, int *b){
  return *(b + a);
}

int main(){
  return f(4, 5);
}
