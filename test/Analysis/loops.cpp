int getNum();

int simple_unroll1(){
  int a[9];
  int k = 12;
  for(int i = 0; i < 9; i++){
    a[i] = 12;
  }
  int b = 22/(k-12); // warning
  return 0;
}

int simple_unroll2(){
  int a[9];
  int k = 12;
  int i;
  for(i = 0; i < 9; i++){
    a[i] = 12;
  }
  int b = 22/(k-12); // warning
  return 0;
}

int nested_outer_unrolled(){
  int a[5];
  int k = 12;
  int j = 0;
  for(int i = 0; i < 9; i++){
    for(j = 0; j<getNum();++j)
    {
      a[2]=22;
    }
    a[3] = 12;
  }
  int b = 22/(k-12); // no warning
  return 0;
}


int nested_inner_unrolled(){
  int a[5];
  int k = 12;
  int j = 0;
  for(int i = 0; i < getNum(); i++){
    for(j = 0; j<8;++j)
    {
      a[2]=22;
    }
    a[3] = 12;
  }
  int b = 22/(k-12); // warning
  return 0;
}

int nested_both_unrolled(){
  int a[5];
  int k = 12;
  int j = 0;
  for(int i = 0; i < 7; i++){
    for(j = 0; j<6;++j)
    {
      a[2]=22;
    }
    a[3] = 12;
  }
  int b = 22/(k-12); // warning
  return 0;
}

