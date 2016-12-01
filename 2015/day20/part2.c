#include <stdio.h>
#include <stdlib.h>

const int MYSMAX = 100000; // choose large enough

int main(int argc, char *argv[]) {

  int *sums = malloc((MYSMAX) * sizeof(int));

  for (int elf = 1; elf < MYSMAX; elf++) {
    for (int k = 1; k <= 50; k++) {
      int at = elf*k;
      if (at >= MYSMAX) break;
      sums[at] += elf*11;
    }
  }

  for (int i = 0; i < MYSMAX; i++) {
    if (sums[i]>=33100000) {
      printf("%d: %d\n", i, sums[i]);
      break;
    }
  }

  return 0;
}

// Run with e.g.:
// gcc -Ofast -Wall part2.c -o main && ./main && rm main
