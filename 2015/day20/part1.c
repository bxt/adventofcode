#include <stdio.h>
#include <stdlib.h>

const int MYSMAX = 2476767;
/*
First we have to see that each house get 10 times the sum of its divisors gifts.
Let s(n) denote the sum of the divisors. MYSMAX is the lowest number for which
s(n) = 3310000. We can quickly calculate this number by hand [1]: First we look at
the factorization of 33100000/10. Since s is multiplicative, that is s(n)*s(m)=s(n*m),
we want to select factors also in the image of s. We peak at the formula from
Berndt 1985 [2] to find possible values, that is numbers (p^n - 1)/(p - 1) for
primes p and n >= 2. Hence we are mostly interested in the even facors f that are
a prime number p plus 1, because those have the property that f = s(p) = p+1 (n=2).
There exist few other feasible factors for other powers like 40 with
s((3^4-1) / (3-1)) = s(40) = 3^4 = 27, but those are hard to compose to 3310000.
There are few possible combinations, since 331 is not in the image of s and we
pick the "best" factors:

662 * 1250 * 4 = 331000, having:
s(661) = 661 + 1 = 662.
s(1249) = 1250
s(3) = 4

By comparing to the other few feasible possibilities we see that those factors
yield the minimum value of 661 * 1249 * 3 = 2476767.

Now that was a fun exercise!

  [1]: See attached script
  [2]: http://mathworld.wolfram.com/DivisorFunction.html#eqn14

*/

int main(int argc, char *argv[]) {

  int *sums = malloc((MYSMAX) * sizeof(int));

  for (int i = 1; i < MYSMAX; i++) {
    for (int k = i; k < MYSMAX; k+=i) {
      sums[k] += i;
    }
  }

  for (int i = 0; i < MYSMAX; i++) {
    if (sums[i]>=3310000) {
      printf("%d: %d\n", i, sums[i]);
      break;
    }
  }

  return 0;
}

// Run with e.g.:
// gcc -Ofast -Wall part1.c -o main && ./main && rm main
