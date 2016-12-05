#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include <openssl/md5.h>

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

const int SEARCH_MAX = 333333333;

int main(int argc, char *argv[]) {

  int (*condition)(unsigned char* r) = startsWithFiveZeros;

  //char key[23] = "abc";
  char key[23] = "ffykfhsq";
  int keyLength = strlen(key);

  char codeOne[] = "        ";
  int codeOneLen = strlen(codeOne);
  char codeTwo[] = "        ";
  int codeTwoLen = strlen(codeTwo);

  printf("Starting Easter Bunny password search...\n");

  int codeOneIndex = 0;
  bool done = 0;
  for (int resultNumber = 0; !done && resultNumber < SEARCH_MAX; resultNumber++) {
    unsigned char result[MD5_DIGEST_LENGTH];
    char resultString[MD5_DIGEST_LENGTH*2 + 1] = {0};

    sprintf(key + keyLength, "%d", resultNumber);
    MD5((unsigned char*) key, strlen(key), result);

    if (condition(result))  {
      stringMd5Sum(result, resultString);
      //printf("\nat: %d\nmd5: %s\n\n", resultNumber, resultString);

      char firstAfterZeros = resultString[5];
      char secondAfterZeros = resultString[6];

      if (codeOneIndex < codeOneLen) {
        codeOne[codeOneIndex++] = firstAfterZeros;
      }

      int codeTwoIndex = firstAfterZeros - '0';
      if (codeTwoIndex < codeTwoLen && codeTwo[codeTwoIndex] == ' ') {
        codeTwo[codeTwoIndex] = secondAfterZeros;
      }

      printf("(%s) (%s)\r", codeOne, codeTwo);
      fflush(stdout);

      done = codeOneIndex >= codeOneLen;
      for (int i = 0; i < codeTwoLen; i++) {
        done = done && codeTwo[i] != ' ';
      }
    }
  }

  printf("\n");

  return 0;
}
