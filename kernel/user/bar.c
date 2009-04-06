void *malloc(int);

main () {
  int i;
  char *j[10];
  for (i = 0; i < 10; i++) {
    printf("%d\n",i);
    j[i] = malloc(1024);
    j[i][512] = 65;
  }
  for (i = 0; i < 10; i++) 
    printf("%c",j[i][512]);
  exit(0);
}

