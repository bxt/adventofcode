#include <stdio.h>
#include <string.h>

const int PLENGTH = 8;

void increment(char* password) {
  for (int i = PLENGTH-1; i >= 0; i--) {
    if (password[i] == 'z') {
      password[i] = 'a';
    } else {
      password[i]++;
      char c = password[i];
      if (c =='o' || c=='i' || c =='l') {
        password[i]++;
      }
      break;
    }
  }
}

int hasStraight(char* password) {
  for (int i = 0; i < PLENGTH-2; i++) {
    if (password[i]+1 == password[i+1] && password[i]+2 == password[i+2]) {
      return 1;
    }
  }
  return 0;
}

int hasPairs(char* password) {
  int pairs[26] = {0};
  int pairCount = 0;
  for (int i = 0; i < PLENGTH-1; i++) {
    if (password[i] == password[i+1]) {
      pairs[password[i]-'a']++;
      if (pairs[password[i]-'a'] == 1) {
        pairCount++;
      }
      if (pairCount == 2) {
        return 1;
      }
      i++;
    }
  }
  return 0;
}

int valid(char* password) {
  return hasStraight(password) && hasPairs(password);
}

void nextPassword(char* password) {
  do {
    increment(password);
  } while(!valid(password));
}

int main(int argc, char *argv[]) {

  char password[PLENGTH+1] = "hepxcrrq";
  nextPassword(password);
  printf("\nPart 1: %s\n", password);
  nextPassword(password);
  printf("\nPart 2: %s\n", password);

  return 0;
}

// Run with e.g.:
// gcc -Ofast -Wall main.c -o main && ./main && rm main
