
int num(int i){
  if(i == 2 || i == 4) return i;
  else return 8 - i;
}

int main(){
  int i;
  for(i = 0; i < 10; i = i + 1){
    print(num(i));
  }
  return 0;
}
