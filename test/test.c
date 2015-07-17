
int logi3(int x, int y) {
  if (x == 0 || x+y < x*y){
      return 1;
  }else{
    return 0;
  }
}

int main(){
  print(logi3(2, 3));
  return 0;
}
