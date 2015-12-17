#include <stdio.h>

const int CLENGTH = 20;
const int EGGNOG = 150;

int main(int argc, char *argv[]) {

  int containers[CLENGTH];

  FILE* fp = fopen("input.txt", "r");
  if(fp == 0) {
    printf("Unable to open input.txt\n");
    return -1;
  }

  for (int i = 0; i < CLENGTH; i++) {
    fscanf(fp, "%d\n", containers + i);
  }

  fclose(fp);

  int dpCache[EGGNOG + 1][CLENGTH + 1] = {{0}};
  dpCache[0][0] = 1;

  for (int i = 0; i < CLENGTH; i++) {
    int size = containers[i];
    for (int k = EGGNOG + 1; k >= size; k--) {
      for (int j = CLENGTH + 1; j >= 1; j--) {
        dpCache[k][j] += dpCache[k - size][j - 1];
      }
    }
  }

  // Dump dpCache:
  /*
  for (int i = 0; i < CLENGTH + 1; i++) {
    for (int k = 0; k < EGGNOG + 1; k++) {
      printf("%d ", dpCache[k][i]);
    }
    printf("\n");
  }
  */

  int possibleTotal = 0;
  int possibleMin = -1;

  for(int i = 0; i < CLENGTH + 1; i++) {
    if(possibleMin < 0 && dpCache[EGGNOG][i] != 0) {
      possibleMin = dpCache[EGGNOG][i];
    }
    possibleTotal += dpCache[EGGNOG][i];
  }

  printf("Part 1: %d\n", possibleTotal);
  printf("Part 2: %d\n", possibleMin);

  return 0;
}

// Run with e.g.:
// gcc -Ofast -Wall main.c -o main && ./main && rm main
