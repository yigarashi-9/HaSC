int array[8];

void initialize(int *array);
void sort(int *array);

int main(){
  int i;
  initialize(array);
  sort(array);
  for(i = 0; i < 8; i = i + 1) print(array[i]);
  return 0;
}

void initialize(int *array){
  int i;
  for(i = 0; i < 8; i = i + 1){
    *(array+i) = num(i);
  }
}

void sort(int *array){
  int i, j, tmp;
  for(i = 0; i < 7; i = i + 1){
    for(j = i+1; j < 8; j = j + 1){
      if(array[i] > array[j]){
	tmp = array[i];
	array[i] = array[j];
	array[j] = tmp;
      }
    }
  }
}

int num(int i){
  if(i == 2 || i == 4) return i;
  else return 8 - i;
}
