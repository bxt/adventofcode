#include <stdio.h>
#include <string.h>

// Note that the input is fixed.

#define Z(x) (((x) < 0) ? 0 : (x))

int main(int argc, char *argv[]) {

  int maxScore1 = 0;
  int maxScore2 = 0;

  for (int sug = 0; sug <= 100; sug++) {
    for (int spr = 0; spr <= 100-sug; spr++) {
      for (int can = 0; can <= 100-sug-spr; can++) {
        for (int cho = 0; cho <= 100-sug-spr-can; cho++) {
          int calories = 2 * sug + 9 * spr + can + 8 * cho;
          int capacity = sug * 3 - spr * 3 - can;
          int durability = 3 * spr;
          int flavor = 4 * can - 2 * cho;
          int texture = 2 * cho - 3 * sug;
          int score = Z(capacity) * durability * Z(flavor) * Z(texture);
          if (score > maxScore1) {
            maxScore1 = score;
          }
          if (calories == 500 && score > maxScore2) {
            maxScore2 = score;
          }
        }
      }
    }
  }

  printf("Max score, part 1: %d\n", maxScore1);
  printf("Max score, part 2: %d\n", maxScore2);

  return 0;
}

// Run with e.g.:
// gcc -Ofast -Wall main.c -o main && ./main && rm main
