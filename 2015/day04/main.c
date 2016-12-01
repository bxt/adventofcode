#include <stdio.h>
#include <string.h>

#include <openssl/md5.h>

// Print the MD5 sum as hex-digits.
// from: http://stackoverflow.com/a/1220177/935676
void print_md5_sum(unsigned char* md) {
  int i;
  for(i=0; i <MD5_DIGEST_LENGTH; i++) {
    printf("%02x",md[i]);
  }
}

int startsWithFiveZeros(unsigned char* r) {
  return r[0] == 0 && r[1] == 0 && r[2] < 16;
}

int startsWithSixZeros(unsigned char* r) {
  return r[0] == 0 && r[1] == 0 && r[2] == 0;
}

int startsWithSevenZeros(unsigned char* r) {
  return r[0] == 0 && r[1] == 0 && r[2] == 0 && r[3] < 16;
}

const int SEARCH_MAX = 333333333;

int main(int argc, char *argv[]) {

  int (*condition)(unsigned char* r) = startsWithSixZeros;
  char key[23] = "bgvyzdsv";
  int keyLength = strlen(key);

  unsigned char result[MD5_DIGEST_LENGTH];
  int resultNumber;

  for (resultNumber = 0; resultNumber < SEARCH_MAX; resultNumber++) {
    sprintf(key + keyLength, "%d", resultNumber);
    MD5((unsigned char*) key, strlen(key), result);
    if (condition(result)) break;
  }

  print_md5_sum(result);
  printf("\nat: %d\n", resultNumber);

  return 0;
}
