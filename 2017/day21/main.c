
// Run with e.g.:
// gcc -Wall -o main main.c && ./main && rm main

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const int initialPattern[3][3] = {
  {0, 1, 0},
  {0, 0, 1},
  {1, 1, 1},
};

const int pattern3Permutations[7][9] = {
  {6, 7, 8, 3, 4, 5, 0, 1, 2}, // mirror X
  {2, 1, 0, 5, 4, 3, 8, 7, 6}, // mirror Y
  {2, 5, 8, 1, 4, 7, 0, 3, 6}, // 90 deg
  {0, 1, 2, 3, 4, 5, 6, 7, 8}, // 180 deg
  {6, 3, 0, 7, 4, 1, 8, 5, 2}, // 270 deg
  {8, 5, 2, 7, 4, 1, 6, 3, 0}, // 90 deg + mirror X
  {0, 3, 6, 1, 4, 7, 2, 5, 8}, // 270 deg + mirror X
};

const int pattern2Permutations[7][4] = {
  {2, 3, 0, 1}, // mirror X
  {1, 0, 3, 2}, // mirror Y
  {1, 3, 0, 2}, // 90 deg
  {0, 1, 2, 3}, // 180 deg
  {2, 0, 3, 1}, // 270 deg
  {3, 1, 2, 0}, // 90 deg + mirror X
  {0, 2, 1, 3}, // 270 deg + mirror X
};

#define MODE_INITIAL 0
#define MODE_RULE3 1
#define MODE_RULE2 2
#define MODE_RULE3_REPLACEMENT 3
#define MODE_RULE2_REPLACEMENT 4

struct rule
{
  uint16_t inputPattern;
  uint16_t outputPattern;
};

struct ruleset
{
  size_t length;
  size_t allocated;
  struct rule *rules;
};

struct ruleset makeRuleset() {
  size_t length = 0;
  size_t allocated = 8;
  struct rule *rules = (struct rule *)malloc(sizeof(struct rule) * allocated);
  struct ruleset ruleset = {length, allocated, rules};
  return ruleset;
}

void appendToRuleset(struct ruleset *ruleset, struct rule rule) {
  if (ruleset->length >= ruleset->allocated) {
    size_t newAllocated = ruleset->allocated * 2;
    struct rule *newRules = (struct rule *)malloc(sizeof(struct rule) * newAllocated);
    memcpy(newRules, ruleset->rules, sizeof(struct rule) * ruleset->length);
    ruleset->allocated = newAllocated;
    ruleset->rules = newRules;
  }
  ruleset->rules[ruleset->length] = rule;
  ruleset->length++;
}

void parseArrow(FILE *fp, char ch, int patternSize) {
  if(ch != ' ') {
    printf("Expected space to start pattern %d arrow, got %c", patternSize, ch);
    exit(EXIT_FAILURE);
  }
  ch = getc(fp);
  if (ch != '=') {
    printf("Expected = in pattern %d arrow, got %c", patternSize, ch);
    exit(EXIT_FAILURE);
  }
  ch = getc(fp);
  if(ch != '>') {
    printf("Expected > in pattern %d arrow, got %c", patternSize, ch);
    exit(EXIT_FAILURE);
  }
  ch = getc(fp);
  if(ch != ' ') {
    printf("Expected space to end pattern %d arrow, got %c", patternSize, ch);
    exit(EXIT_FAILURE);
  }
}

int countPattern(size_t patternSize, int *patternData) {
  int count = 0;
  for (size_t i = 0; i < patternSize; i++) {
    for (size_t k = 0; k < patternSize; k++) {
      count += patternData[i * patternSize + k] ;
    }
  }
  return count;
}

int main(int argc, char const *argv[])
{
  struct ruleset rule2s = makeRuleset();
  struct ruleset rule3s = makeRuleset();

  FILE *fp;
  fp = fopen("input.txt", "r");
  if (fp == NULL) exit(EXIT_FAILURE);

  int mode = MODE_INITIAL;
  int patternIndex = 0;
  uint16_t pattern = 0;
  int replacementIndex = 0;
  uint16_t replacement = 0;

  for (char ch = getc(fp); ch != EOF; ch = getc(fp)) {
    if (mode == MODE_INITIAL) {
      if (ch == '.') {
        pattern <<= 1;
        patternIndex++;
      } else if (ch == '#') {
        pattern <<= 1;
        pattern |= 1;
        patternIndex++;
      } else if (ch == '/') {
        if (patternIndex == 2) {
          mode = MODE_RULE2;
        } else if (patternIndex == 3) {
          mode = MODE_RULE3;
        } else {
          printf("Expected / at index 2 or 3, got %d", patternIndex);
          exit(EXIT_FAILURE);
        }
      } else {
        printf("Expected . or # at start of pattern, got %c", ch);
        exit(EXIT_FAILURE);
      }
    } else if (mode == MODE_RULE2) {
      if (patternIndex >= 4) {
        parseArrow(fp, ch, 2);
        mode = MODE_RULE2_REPLACEMENT;
      } else if (ch == '.') {
        pattern <<= 1;
        patternIndex++;
      } else if (ch == '#') {
        pattern <<= 1;
        pattern |= 1;
        patternIndex++;
      } else {
        printf("Expected . or # in rule 2 pattern, got %c", ch);
        exit(EXIT_FAILURE);
      }
    } else if (mode == MODE_RULE2_REPLACEMENT) {
      if (replacementIndex >= 9) {
        if (ch == '\n') {
          struct rule rule2 = {
              pattern,
              replacement,
          };
          appendToRuleset(&rule2s, rule2);
          mode = MODE_INITIAL;
          patternIndex = 0;
          pattern = 0;
          replacementIndex = 0;
          replacement = 0;
        } else {
          printf("Expected new line at end of rule 2, got %c", ch);
          exit(EXIT_FAILURE);
        }
      } else {
        if (replacementIndex == 3 || replacementIndex == 6) {
          if (ch == '/') {
            ch = getc(fp);
          } else {
            printf("Expected / before pattern replacement index %d, got %c", replacementIndex, ch);
            exit(EXIT_FAILURE);
          }
        }
        if (ch == '.') {
          replacement <<= 1;
          replacementIndex++;
        } else if (ch == '#') {
          replacement <<= 1;
          replacement |= 1;
          replacementIndex++;
        } else {
          printf("Expected . or # in rule 2 replacement, got %c", ch);
          exit(EXIT_FAILURE);
        }
      }
    } else if (mode == MODE_RULE3) {
      if (patternIndex >= 9) {
        parseArrow(fp, ch, 3);
        mode = MODE_RULE3_REPLACEMENT;
      } else {
        if (patternIndex == 6) {
          if (ch == '/') {
            ch = getc(fp);
          } else {
            printf("Expected / before pattern index %d, got %c", patternIndex, ch);
            exit(EXIT_FAILURE);
          }
        }
        if (ch == '.') {
          pattern <<= 1;
          patternIndex++;
        } else if (ch == '#') {
          pattern <<= 1;
          pattern |= 1;
          patternIndex++;
        } else {
          printf("Expected . or # in rule 3 pattern, got %c", ch);
          exit(EXIT_FAILURE);
        }
      }
    } else if (mode == MODE_RULE3_REPLACEMENT) {
      if (replacementIndex >= 16) {
        if (ch == '\n') {
          struct rule rule3 = {
              pattern,
              replacement,
          };
          appendToRuleset(&rule3s, rule3);
          mode = MODE_INITIAL;
          patternIndex = 0;
          pattern = 0;
          replacementIndex = 0;
          replacement = 0;
        } else {
          printf("Expected new line at end of rule 3, got %c", ch);
          exit(EXIT_FAILURE);
        }
      } else {
        if (replacementIndex != 0 && replacementIndex % 4 == 0) {
          if (ch == '/') {
            ch = getc(fp);
          } else {
            printf("Expected / before pattern replacement index %d, got %c", replacementIndex, ch);
            exit(EXIT_FAILURE);
          }
        }
        if (ch == '.') {
          replacement <<= 1;
          replacementIndex++;
        } else if (ch == '#') {
          replacement <<= 1;
          replacement |= 1;
          replacementIndex++;
        } else {
          printf("Expected . or # in rule 3 replacement, got %c", ch);
          exit(EXIT_FAILURE);
        }
      }
    }
  }

  size_t rule3sWithoutRotation = rule3s.length;
  for (size_t i = 0; i < rule3sWithoutRotation; i++) {
    for (int k = 0; k < 7; k++) {
      uint16_t pattern = rule3s.rules[i].inputPattern;
      uint16_t permutedPattern = 0;
      for (int l = 0; l < 9; l++) {
        permutedPattern <<= 1;
        if ((pattern & (1 << pattern3Permutations[k][l])) > 0) {
          permutedPattern |= 1;
        }
      }

      struct rule rule3 = {
          permutedPattern,
          rule3s.rules[i].outputPattern,
      };
      appendToRuleset(&rule3s, rule3);
    }
  }

  size_t rule2sWithoutRotation = rule2s.length;
  for (size_t i = 0; i < rule2sWithoutRotation; i++) {
    for (int k = 0; k < 7; k++) {
      uint16_t pattern = rule2s.rules[i].inputPattern;
      uint16_t permutedPattern = 0;
      for (int l = 0; l < 4; l++) {
        permutedPattern <<= 1;
        if ((pattern & (1 << pattern2Permutations[k][l])) > 0) {
          permutedPattern |= 1;
        }
      }

      struct rule rule2 = {
          permutedPattern,
          rule2s.rules[i].outputPattern,
      };
      appendToRuleset(&rule2s, rule2);
    }
  }

  size_t patternSize = 3;
  int *patternData = (int *)malloc(sizeof(int) * patternSize * patternSize);
  memcpy(patternData, initialPattern, sizeof(int) * patternSize * patternSize);

  for (int iteration = 0; iteration < 18; iteration++) {
    if(iteration == 5) {
      printf("Result part 1: %d\n", countPattern(patternSize, patternData));
    }

    int patchSize = patternSize % 2 == 0 ? 2 : 3;
    int newPatchSize = patchSize == 2 ? 3 : 4;

    struct ruleset ruleset = patchSize == 2 ? rule2s : rule3s;

    int patchCount = patternSize / patchSize;

    size_t newPatternSize = patchCount * newPatchSize;
    int *newPatternData = (int *)malloc(sizeof(int) * newPatternSize * newPatternSize);

    for (int i = 0; i < patchCount; i++) {
      for (int k = 0; k < patchCount; k++) {
        uint16_t pattern = 0;
        for (int pi = 0; pi < patchSize; pi++) {
          for (int pk = 0; pk < patchSize; pk++) {
            pattern |= patternData[(i * patchSize + pi) * patternSize + (k * patchSize + pk)]
              << ((patchSize - pi) * patchSize - pk - 1);
          }
        }

        uint16_t replacement = 0;
        for (size_t i = 0; i < ruleset.length; i++) {
          if(ruleset.rules[i].inputPattern == pattern) {
            replacement = ruleset.rules[i].outputPattern;
          }
        }

        for (int pi = 0; pi < newPatchSize; pi++) {
          for (int pk = 0; pk < newPatchSize; pk++) {
            newPatternData[(i * newPatchSize + pi) * newPatternSize + (k * newPatchSize + pk)] =
              (replacement >> ((newPatchSize - pi) * newPatchSize - pk - 1)) & 1;
          }
        }
      }
    }

    patternSize = newPatternSize;
    patternData = newPatternData;
  }

  printf("Result part 2: %d\n", countPattern(patternSize, patternData));

  // FIXME: free memories again :)

  return 0;
}
