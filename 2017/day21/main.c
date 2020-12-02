#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const int pattern3Permutations[5][9] = {
  {6, 7, 8, 3, 4, 5, 0, 1, 2},
  {2, 1, 0, 5, 4, 3, 8, 7, 6},
  {2, 5, 8, 1, 4, 7, 0, 3, 6},
  {0, 1, 2, 3, 4, 5, 6, 7, 8},
  {6, 3, 0, 7, 4, 1, 8, 5, 2},
};

const int pattern2Permutations[5][4] = {
  {2, 3, 0, 1},
  {1, 0, 3, 2},
  {1, 3, 0, 2},
  {0, 1, 2, 3},
  {2, 0, 3, 1},
};

#define MODE_INITIAL 0
#define MODE_RULE3 1
#define MODE_RULE2 2
#define MODE_RULE3_REPLACEMENT 3
#define MODE_RULE2_REPLACEMENT 4

struct rule3
{
  uint16_t inputPattern;
  uint16_t outputPattern;
};

struct rule2
{
  uint8_t inputPattern;
  uint16_t outputPattern;
};

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

int main(int argc, char const *argv[])
{
  size_t rule3sCount = 8;
  struct rule3 *rule3s = (struct rule3 *)malloc(sizeof(struct rule3) * rule3sCount);

  size_t rule2sCount = 8;
  struct rule2 *rule2s = (struct rule2 *)malloc(sizeof(struct rule2) * rule2sCount);

  FILE *fp;
  fp = fopen("input.txt", "r");
  if (fp == NULL) exit(EXIT_FAILURE);

  int mode = MODE_INITIAL;
  int patternIndex = 0;
  uint16_t pattern = 0;
  int replacementIndex = 0;
  uint16_t replacement = 0;

  size_t rule3sIndex = 0;
  size_t rule2sIndex = 0;

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
          printf("PARSED RULE 2 %d, %d\n", pattern, replacement);
          if (rule2sIndex >= rule2sCount) {
            size_t newRule2sCount = rule2sCount * 2;
            struct rule2 *newRule2s = (struct rule2 *)malloc(sizeof(struct rule2) * newRule2sCount);
            memcpy(newRule2s, rule2s, sizeof(struct rule2) * rule2sIndex);
            rule2sCount = newRule2sCount;
            rule2s = newRule2s;
          }
          struct rule2 rule2 = {
              (uint8_t)pattern,
              replacement,
          };
          rule2s[rule2sIndex] = rule2;
          rule2sIndex++;
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
          printf("PARSED RULE 3 %d, %d\n", pattern, replacement);
          if (rule3sIndex >= rule3sCount) {
            size_t newRule3sCount = rule3sCount * 2;
            struct rule3 *newRule3s = (struct rule3 *)malloc(sizeof(struct rule3) * newRule3sCount);
            memcpy(newRule3s, rule3s, sizeof(struct rule3) * rule3sIndex);
            rule3sCount = newRule3sCount;
            rule3s = newRule3s;
          }
          struct rule3 rule3 = {
              pattern,
              replacement,
          };
          rule3s[rule3sIndex] = rule3;
          rule3sIndex++;
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

  size_t rule3sWithoutRotation = rule3sIndex;
  for (size_t i = 0; i < rule3sWithoutRotation; i++) {
    for (int k = 0; k < 5; k++) {
      if (rule3sIndex >= rule3sCount) {
        size_t newRule3sCount = rule3sCount * 2;
        struct rule3 *newRule3s = (struct rule3 *)malloc(sizeof(struct rule3) * newRule3sCount);
        memcpy(newRule3s, rule3s, sizeof(struct rule3) * rule3sIndex);
        rule3sCount = newRule3sCount;
        rule3s = newRule3s;
      }

      uint16_t pattern = rule3s[i].inputPattern;
      uint16_t permutedPattern = 0;
      for (int l = 0; l < 9; l++) {
        permutedPattern <<= 1;
        if ((pattern & (1 << pattern3Permutations[k][l])) > 0) {
          permutedPattern |= 1;
        }
      }

      struct rule3 rule3 = {
          permutedPattern,
          rule3s[i].outputPattern,
      };
      rule3s[rule3sIndex] = rule3;
      rule3sIndex++;
    }
  }

  size_t rule2sWithoutRotation = rule2sIndex;
  for (size_t i = 0; i < rule2sWithoutRotation; i++) {
    for (int k = 0; k < 5; k++) {
      if (rule2sIndex >= rule2sCount) {
        size_t newRule2sCount = rule2sCount * 2;
        struct rule2 *newRule2s = (struct rule2 *)malloc(sizeof(struct rule2) * newRule2sCount);
        memcpy(newRule2s, rule2s, sizeof(struct rule2) * rule2sIndex);
        rule2sCount = newRule2sCount;
        rule2s = newRule2s;
      }

      uint8_t pattern = rule2s[i].inputPattern;
      uint8_t permutedPattern = 0;
      for (int l = 0; l < 4; l++) {
        permutedPattern <<= 1;
        if ((pattern & (1 << pattern2Permutations[k][l])) > 0) {
          permutedPattern |= 1;
        }
      }

      struct rule2 rule2 = {
          permutedPattern,
          rule2s[i].outputPattern,
      };
      rule2s[rule2sIndex] = rule2;
      rule2sIndex++;
    }
  }

  for (size_t i = 0; i < rule2sIndex; i++) {
    printf("Has rule 2 with %d, %d\n", rule2s[i].inputPattern, rule2s[i].outputPattern);
  }
  for (size_t i = 0; i < rule3sIndex; i++) {
    printf("Has rule 3 with %d, %d\n", rule3s[i].inputPattern, rule3s[i].outputPattern);
  }


  return 0;
}
