#include <stdio.h>

int main(int argc, char *argv[]) {

  int basementAfter = 0;
  int searchingBasement = 1;
  int floor = 0;

  FILE* fp = fopen("input.txt", "r");
  if(fp == 0) {
    printf("Unable to open input.txt\n");
    return -1;
  }

  static unsigned char buf[4096];
  size_t n;
  while ((n = fread(buf, 1, sizeof(buf), fp)) > 0) {
    for (size_t i = 0; i < n; i++) {
      char c = buf[i];
      if (c == '(') {
        floor++;
      }
      if (c == ')') {
        floor--;
      }
      if (searchingBasement) {
        basementAfter++;
        searchingBasement = floor != -1;
      }
    }
  }

  fclose(fp);

  printf("Part 1: %d\n", floor);
  printf("Part 2: %d\n", basementAfter);

  return 0;
}

// 1797 / 280

// Run with e.g.:
// gcc -Ofast -Wall main.c -o main && ./main && rm main
