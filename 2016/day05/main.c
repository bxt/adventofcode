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
  return !(r[0] | r[1] | (r[2] & 0xf0));
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

int main(int argc, char *argv[]) {

  MD5_CTX context;
  MD5_Init(&context);

  char key[23] = "ffykfhsq";
  char* keyEnd = key + strlen(key);
  char* end = keyEnd + 1;
  keyEnd[0] = '0';

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

    //sprintf(key + keyLength, "%d", resultNumber);
    MD5_Update(&context, (unsigned char*) key, end - key);
    MD5_Final(result, &context);

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

    for (char* p = end - 1; ; *p-- = '0') {
      if (*p != '9') {
        (*p)++;
        break;
      }
      if (p == keyEnd) {
        *p = '1';
        *end++ = '0';
        break;
      }
    }
  }

  printf("\n");

  return 0;
}
