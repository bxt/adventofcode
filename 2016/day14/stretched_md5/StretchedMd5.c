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
    into[i*2+0] = digit0 + '0' + (digit0 >= 10) * ('a' - '0' - 10);
    into[i*2+1] = digit1 + '0' + (digit1 >= 10) * ('a' - '0' - 10);
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
