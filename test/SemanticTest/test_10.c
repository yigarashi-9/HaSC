void f(int a){
  return;
}

int main(){
  int b;
  b = 34;
  while(f(b)){
    return b;
  }else{
    return b+1;
  }
}
