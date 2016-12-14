#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include <openssl/md5.h>

#include "ruby.h"

VALUE StretchedMd5 = Qnil;

void Init_stretched_md5();

VALUE method_get(VALUE self, VALUE arg1);

void Init_stretched_md5() {
	StretchedMd5 = rb_define_module("StretchedMd5");
	rb_define_singleton_method(StretchedMd5, "get", method_get, 1);
}

void stringMd5Sum(unsigned char* md, char* into) {
  int i;
  for(i = 0; i < MD5_DIGEST_LENGTH; i++) {
    unsigned char digit0 = md[i] >> 4;
    unsigned char digit1 = md[i] & 0x0f;
    into[i*2+0] = digit0 < 10 ? digit0 + '0' : digit0 - 10 + 'a';
    into[i*2+1] = digit1 < 10 ? digit1 + '0' : digit1 - 10 + 'a';
  }
}

VALUE method_get(VALUE self, VALUE arg1) {
  const int resultStringLength = MD5_DIGEST_LENGTH*2;
  char resultString[resultStringLength + 1] = {0};
  unsigned char result[MD5_DIGEST_LENGTH];

  char* key = StringValueCStr(arg1);
  MD5((unsigned char*) key, strlen(key), result);
  stringMd5Sum(result, resultString);

  for (int i = 0; i < 2016; i++) {
    MD5((unsigned char*) resultString, resultStringLength, result);
    stringMd5Sum(result, resultString);
  }

  VALUE resultRuby = rb_str_new(resultString, resultStringLength);

	return resultRuby;
}

// int main(int argc, char *argv[]) {
//   //char key[23] = "abc";
//   char key[23] = "yjdafjpo";
//   size_t keyLength = strlen(key);
//
//   bool done = false;
//   for (int resultNumber = 0; !done && resultNumber < SEARCH_MAX; resultNumber++) {
//     unsigned char result[MD5_DIGEST_LENGTH];
//
//     int resultLength = sprintf(key + keyLength, "%d", resultNumber);
//     MD5((unsigned char*) key, resultLength + keyLength, result);
//
//     char resultString[MD5_DIGEST_LENGTH*2 + 1] = {0};
//     stringMd5Sum(result, resultString);
//     //printf("\nat: %d\nmd5: %s\n\n", resultNumber, resultString);
//     char firstAfterZeros = resultString[5];
//     char secondAfterZeros = resultString[6];
//
//     printf("%s %d\n", resultNumber, resultString);
//   }
//
//   return 0;
// }
