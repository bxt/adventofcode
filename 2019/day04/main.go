package main

import (
	"fmt"
	"reflect"
)

func assertEquals(expected any, actual any) {
	if !reflect.DeepEqual(expected, actual) {
		panic(fmt.Sprintf(
			"expected = %[1]v : %[1]T != actual = %[2]v : %[2]T",
			expected, actual))
	}
}

func meetsCriteria(n int) bool {
	previousDigit := 10
	foundSame := false
	for n > 0 {
		digit := n % 10
		if digit == previousDigit {
			foundSame = true
		}
		if digit > previousDigit {
			return false
		}
		previousDigit = digit
		n /= 10
	}
	return foundSame
}

func meetsCriteriaPart2(n int) bool {
	previousPreviousPreviousDigit := 10
	previousPreviousDigit := 10
	previousDigit := 10
	foundPair := false
	for n > 0 {
		digit := n % 10
		if digit != previousDigit &&
			previousDigit == previousPreviousDigit &&
			previousPreviousDigit != previousPreviousPreviousDigit {
			foundPair = true
		}
		if digit > previousDigit {
			return false
		}
		previousPreviousPreviousDigit = previousPreviousDigit
		previousPreviousDigit = previousDigit
		previousDigit = digit
		n /= 10
	}
	return foundPair || (previousDigit == previousPreviousDigit &&
		previousPreviousDigit != previousPreviousPreviousDigit)
}

func main() {
	assertEquals(true, meetsCriteria(111111))
	assertEquals(true, meetsCriteria(122345))
	assertEquals(true, meetsCriteria(111123))
	assertEquals(false, meetsCriteria(223450))
	assertEquals(false, meetsCriteria(123789))

	// Puzzle input:
	from, to := 271973, 785961

	var count int

	count = 0
	for i := from; i <= to; i++ {
		if meetsCriteria(i) {
			count++
		}
	}

	fmt.Printf("Part 1: %d\n", count) // 925

	assertEquals(true, meetsCriteriaPart2(112233))
	assertEquals(false, meetsCriteriaPart2(123444))
	assertEquals(true, meetsCriteriaPart2(111122))

	count = 0
	for i := from; i <= to; i++ {
		if meetsCriteriaPart2(i) {
			count++
		}
	}

	fmt.Printf("Part 2: %d\n", count) // 607
}
