#include <stdio.h>
#include <string.h> /* memset */

const int EGGNOG = 150;

int main(int argc, char *argv[]) {

  FILE* fp = fopen("input.txt", "r");
  if(fp == 0) {
    printf("Unable to open input.txt\n");
    return -1;
  }

  int cLength = 0;
  while (fscanf(fp, "%*d\n") != EOF) cLength++;
  rewind(fp);

  int containers[cLength];
  for (int i = 0; i < cLength; i++) {
    fscanf(fp, "%d\n", containers + i);
  }

  fclose(fp);

  /* We use dynamic programming to cache the number of possibilities for each
  amount of eggnog and for the number of containers used. This way we sacrifice
  O(eggnog * #containers) memory and get O(eggnog * #containers^2) runtime as
  opposed to about O(2^#containers) runtime. For the 20 containers in the input
  this does not matter at all, but at 50 containers, our Haskell solution takes
  about 35 seconds, while the C solution still only takes about 10ms. */
  int dpCache[EGGNOG + 1][cLength + 1];
  memset(dpCache, 0, (EGGNOG + 1)*(cLength + 1)*sizeof(int));
  dpCache[0][0] = 1;

  for (int i = 0; i < cLength; i++) {
    int size = containers[i];
    for (int k = EGGNOG + 1; k >= size; k--) {
      for (int j = cLength + 1; j >= 1; j--) {
        dpCache[k][j] += dpCache[k - size][j - 1];
      }
    }
  }

  // Dump dpCache:
  /*
  for (int i = 0; i < cLength + 1; i++) {
    for (int k = 0; k < EGGNOG + 1; k++) {
      printf("%d ", dpCache[k][i]);
    }
    printf("\n");
  }
  */

  int possibleTotal = 0;
  int possibleMin = -1;

  for(int i = 0; i < cLength + 1; i++) {
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
