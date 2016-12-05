#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include <openssl/md5.h>

const char PLACEHOLDER = '_';
const int SEARCH_MAX = 333333333;

void stringMd5Sum(unsigned char* md, char* into) {
  int i;
  for(i = 0; i < MD5_DIGEST_LENGTH; i++) {
    //printf("%02x", md[i]);
    sprintf(into + i*2, "%02x", md[i]);
  }
}

int startsWithFiveZeros(unsigned char* r) {
  return r[0] == 0 && r[1] == 0 && r[2] < 16;
}

bool codeUpdateOne(char* code, char firstAfterZeros, char secondAfterZeros) {
  char* firstSpace = strchr(code, PLACEHOLDER);
  if (firstSpace) {
    *firstSpace = firstAfterZeros;
    return firstSpace == code + strlen(code);
  }
  return true;
}

bool codeUpdateTwo(char* code, char firstAfterZeros, char secondAfterZeros) {
  int len = strlen(code);
  int index = firstAfterZeros - '0';
  if (index < len && code[index] == PLACEHOLDER) {
    code[index] = secondAfterZeros;
  }

  bool done = true;
  for (int i = 0; i < len; i++) {
    done = done && code[i] != PLACEHOLDER;
  }
  return done;
}

typedef bool (*updater)(char* code, char firstAfterZeros, char secondAfterZeros);

struct codeEnv {
   bool done;
   char code[9];
   updater updater;
};

int main(int argc, char *argv[]) {
  //char key[23] = "abc";
  char key[23] = "ffykfhsq";
  int keyLength = strlen(key);

  struct codeEnv codeEnvs[] = {
    {false, "________", codeUpdateOne},
    {false, "________", codeUpdateTwo}
  };
  int codeEnvLenght = 2;

  printf("Starting Easter Bunny password search...\n");

  bool done = false;
  for (int resultNumber = 0; !done && resultNumber < SEARCH_MAX; resultNumber++) {
    unsigned char result[MD5_DIGEST_LENGTH];

    sprintf(key + keyLength, "%d", resultNumber);
    MD5((unsigned char*) key, strlen(key), result);

    if (startsWithFiveZeros(result))  {
      char resultString[MD5_DIGEST_LENGTH*2 + 1] = {0};
      stringMd5Sum(result, resultString);
      //printf("\nat: %d\nmd5: %s\n\n", resultNumber, resultString);
      char firstAfterZeros = resultString[5];
      char secondAfterZeros = resultString[6];

      done = true;
      for(int i = 0; i < codeEnvLenght; i++) {
        struct codeEnv* codeEnv = codeEnvs + i;
        if (!codeEnv->done) {
          codeEnv->done = codeEnv->updater(codeEnv->code, firstAfterZeros, secondAfterZeros);
        }
        printf("(%s) ", codeEnv->code);
        done = done && codeEnv->done;
      }

      printf("@ %d\r", resultNumber);
      fflush(stdout);
    }
  }

  printf("\n");

  return 0;
}
