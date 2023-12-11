package main

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"os"
	"reflect"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func assertEquals(expected interface{}, actual interface{}) {
	if !reflect.DeepEqual(expected, actual) {
		panic(fmt.Sprintf(
			"expected = %[1]v : %[1]T != actual = %[2]v : %[2]T",
			expected, actual))
	}
}

func main() {
	width := 25
	height := 6

	file, err := os.Open("input.txt")
	check(err)
	defer file.Close()

	reader := bufio.NewReader(file)
	min_number_of_zeros := -1
	result := -1

	// find the layer that contains the fewest 0 digits
	// On that layer, what is the number of 1 digits multiplied by the number of 2 digits?
reading_layers:
	for {
		number_of_zeros := 0
		number_of_ones := 0
		number_of_twos := 0

		for i := 0; i < height; i++ {
			for i := 0; i < width; i++ {
				b, err := reader.ReadByte()
				if errors.Is(err, io.EOF) {
					break reading_layers
				} else {
					check(err)
				}

				if b == '0' {
					number_of_zeros += 1
				}
				if b == '1' {
					number_of_ones += 1
				}
				if b == '2' {
					number_of_twos += 1
				}

				// fmt.Printf("-%s-\n", string(b))
			}
		}

		// fmt.Printf("0: %d, 1: %d, 2: %d\n", number_of_zeros, number_of_ones, number_of_twos)

		if min_number_of_zeros == -1 || number_of_zeros < min_number_of_zeros {
			min_number_of_zeros = number_of_zeros
			result = number_of_ones * number_of_twos
		}

	}

	fmt.Printf("Part 1: %d\n", result)
}
