#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include <openssl/md5.h>

const char PLACEHOLDER = '*';
const int SEARCH_MAX = 333333333;
const int CODE_LENGTH = 8;

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
    return firstSpace == code + strlen(code) - 1;
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
   char code[CODE_LENGTH + 1];
   updater updater;
};

struct codeEnv makeCodeEnv(updater updater) {
  struct codeEnv result = {false, {0}, updater};
  for(int i = 0; i < CODE_LENGTH; i++) {
    result.code[i] = PLACEHOLDER;
  }
  return result;
}

size_t stringInt(int number, char* into) {
  static char numberStr[128] = { 0 };

  size_t i = 0;
  while (number != 0) {
      numberStr[i++] = (number % 10) + '0';
      number /= 10;
  }
  size_t numberLen = i;

  char* end = into + numberLen;
  while (i > 0) {
    i--;
    end[-i-1] = numberStr[i];
  }
  end[0] = '\0';

  return numberLen;
}

int main(int argc, char *argv[]) {

  //char key[23] = "abc";
  char key[23] = "ffykfhsq";
  size_t keyLength = strlen(key);

  struct codeEnv codeEnvs[] = {
    makeCodeEnv(codeUpdateOne),
    makeCodeEnv(codeUpdateTwo),
  };
  int codeEnvLenght = sizeof(codeEnvs)/sizeof(struct codeEnv);

  printf("Starting Easter Bunny password search...\n");

  bool done = false;
  for (int resultNumber = 3; !done && resultNumber < SEARCH_MAX; resultNumber++) {
    unsigned char result[MD5_DIGEST_LENGTH];

    //printf("\nNS: %s, NL: %d\n", numberStr, numberLen);

    //printf("\nkey: %s\n", key);

    size_t numberLen = stringInt(resultNumber, key + keyLength);
    //sprintf(key + keyLength, "%d", resultNumber);
    MD5((unsigned char*) key, keyLength + numberLen, result);

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
