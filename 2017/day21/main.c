
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

#define PERMUTATION_COUNT 7

const int pattern3Permutations[PERMUTATION_COUNT][9] = {
  {6, 7, 8, 3, 4, 5, 0, 1, 2}, // mirror X
  {2, 1, 0, 5, 4, 3, 8, 7, 6}, // mirror Y
  {2, 5, 8, 1, 4, 7, 0, 3, 6}, // 90 deg
  {0, 1, 2, 3, 4, 5, 6, 7, 8}, // 180 deg
  {6, 3, 0, 7, 4, 1, 8, 5, 2}, // 270 deg
  {8, 5, 2, 7, 4, 1, 6, 3, 0}, // 90 deg + mirror X
  {0, 3, 6, 1, 4, 7, 2, 5, 8}, // 270 deg + mirror X
};

const int pattern2Permutations[PERMUTATION_COUNT][4] = {
  {2, 3, 0, 1}, // mirror X
  {1, 0, 3, 2}, // mirror Y
  {1, 3, 0, 2}, // 90 deg
  {0, 1, 2, 3}, // 180 deg
  {2, 0, 3, 1}, // 270 deg
  {3, 1, 2, 0}, // 90 deg + mirror X
  {0, 2, 1, 3}, // 270 deg + mirror X
};

#define MODE_RULE_PATTERN 1
#define MODE_RULE_REPLACEMENT 2

struct rule {
  uint16_t inputPattern;
  uint16_t outputPattern;
};

struct ruleset {
  int patternSize;
  size_t length;
  size_t allocated;
  struct rule *rules;
};

struct ruleset makeRuleset(int patternSize) {
  size_t length = 0;
  size_t allocated = 8;
  struct rule *rules = (struct rule *)malloc(sizeof(struct rule) * allocated);
  struct ruleset ruleset = {patternSize, length, allocated, rules};
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

void addPermutationsToRuleset(struct ruleset *ruleset, const int *patternPermutations) {
  size_t rulesWithoutRotation = ruleset->length;
  int patternSizeSquared = ruleset->patternSize * ruleset->patternSize;
  for (size_t i = 0; i < rulesWithoutRotation; i++) {
    for (int k = 0; k < PERMUTATION_COUNT; k++) {
      uint16_t pattern = ruleset->rules[i].inputPattern;
      uint16_t permutedPattern = 0;
      for (int l = 0; l < patternSizeSquared; l++) {
        permutedPattern <<= 1;
        if ((pattern & (1 << patternPermutations[k * patternSizeSquared + l])) > 0) {
          permutedPattern |= 1;
        }
      }
      struct rule rule = {
          permutedPattern,
          ruleset->rules[i].outputPattern,
      };
      appendToRuleset(ruleset, rule);
    }
  }
}

struct scanner {
  FILE *fp;
  char ch;
  int line;
  int column;
};

struct scanner makeScanner(const char *fileName) {
  FILE *fp;
  fp = fopen(fileName, "r");
  if (fp == NULL) exit(EXIT_FAILURE);

  char ch = getc(fp);

  int line = 1;
  int column = 0;

  struct scanner scanner = {fp, ch, line, column};
  return scanner;
}

void advanceScanner(struct scanner *scanner) {
  if (scanner->ch == '\n') {
    scanner->line++;
    scanner->column = 0;
  } else {
    scanner->column++;
  }
  scanner->ch = getc(scanner->fp);
}

void printPositionOfScanner(struct scanner *scanner) {
  printf(" (line %d, column %d)\n", scanner->line, scanner->column);
}

void advanceScannerIf(struct scanner *scanner, char ch, const char *context) {
  if (scanner->ch != ch) {
    printf("Expected %c %s, got %c", ch, context, scanner->ch);
    printPositionOfScanner(scanner);
    exit(EXIT_FAILURE);
  }

  advanceScanner(scanner);
}

void advanceScannerOver(struct scanner *scanner, const char *string, const char *context) {
  while(*string != '\0') {
    advanceScannerIf(scanner, *string, context);
    string++;
  }
}

void parseArrow(struct scanner *scanner) {
  advanceScannerOver(scanner, " => ", "in pattern arrow");
}

int parseBinaryDigit(struct scanner *scanner, const char *context) {
  if (scanner->ch == '.') {
    advanceScanner(scanner);
    return 0;
  } else if (scanner->ch == '#') {
    advanceScanner(scanner);
    return 1;
  } else {
    printf("Expected . or # in binary digit %s, got %c", context, scanner->ch);
    printPositionOfScanner(scanner);
    exit(EXIT_FAILURE);
  }
}

void parseRulePattern(struct scanner *scanner, int *patternSize, uint16_t *pattern) {
  *patternSize = -1;
  *pattern = 0;
  int patternIndex = 0;

  while (*patternSize < 0 || patternIndex < *patternSize * *patternSize) {
    if (*patternSize < 0) {
      if(scanner->ch == '/') {
        if (patternIndex == 2) {
          *patternSize = 2;
        } else if (patternIndex == 3) {
          *patternSize = 3;
        } else {
          printf("Expected / at index 2 or 3, got %d", patternIndex);
          printPositionOfScanner(scanner);
          exit(EXIT_FAILURE);
        }
      } else {
        int digit = parseBinaryDigit(scanner, "at start of pattern");
        *pattern <<= 1;
        *pattern |= digit;
        patternIndex++;
      }
    } else {
      if (patternIndex % *patternSize == 0) {
        advanceScannerIf(scanner, '/', "before pattern");
      }
      int digit = parseBinaryDigit(scanner, "in rule pattern");
      *pattern <<= 1;
      *pattern |= digit;
      patternIndex++;
    }
  }
}
void parseRuleReplacement(struct scanner *scanner, int replacementSize, uint16_t *replacement) {
  int replacementIndex = 0;
  *replacement = 0;

  while (replacementIndex < replacementSize * replacementSize) {
    if (replacementIndex != 0 && replacementIndex % replacementSize == 0) {
      advanceScannerIf(scanner, '/', "before pattern replacement");
    }
    int digit = parseBinaryDigit(scanner, "in rule replacement");
    *replacement <<= 1;
    *replacement |= digit;
    replacementIndex++;
  }
}

void parseRule(struct scanner *scanner, struct ruleset *rule2s, struct ruleset *rule3s) {
  int patternSize;
  uint16_t pattern;
  parseRulePattern(scanner, &patternSize, &pattern);

  parseArrow(scanner);

  int replacementSize;
  if (patternSize == 2) {
    replacementSize = 3;
  } else {
    replacementSize = 4;
  }
  uint16_t replacement;
  parseRuleReplacement(scanner, replacementSize, &replacement);

  advanceScannerIf(scanner, '\n', "at end of rule");

  struct rule rule = {
      pattern,
      replacement,
  };
  if (patternSize == 2) {
    appendToRuleset(rule2s, rule);
  } else {
    appendToRuleset(rule3s, rule);
  }
}

void parseRules(struct scanner *scanner, struct ruleset *rule2s, struct ruleset *rule3s) {
  while (scanner->ch != EOF) {
    parseRule(scanner, rule2s, rule3s);
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
  struct ruleset rule2s = makeRuleset(2);
  struct ruleset rule3s = makeRuleset(3);

  struct scanner scanner = makeScanner("input.txt");
  parseRules(&scanner, &rule2s, &rule3s);

  addPermutationsToRuleset(&rule2s, pattern2Permutations);
  addPermutationsToRuleset(&rule3s, pattern3Permutations);

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
