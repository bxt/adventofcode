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

func assertEquals(expected any, actual any) {
	if !reflect.DeepEqual(expected, actual) {
		panic(fmt.Sprintf(
			"expected = %[1]v : %[1]T != actual = %[2]v : %[2]T",
			expected, actual))
	}
}

const width = 25
const height = 6

func main() {
	var image [width][height]int

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

		for y := range height {
			for x := range width {
				b, err := reader.ReadByte()
				if errors.Is(err, io.EOF) {
					break reading_layers
				} else {
					check(err)
				}

				if b == '0' { // black
					number_of_zeros += 1
					if image[x][y] == 0 {
						image[x][y] = 3 // output black
					}
				}
				if b == '1' { // white
					number_of_ones += 1
					if image[x][y] == 0 {
						image[x][y] = 4 // output white
					}
				}
				if b == '2' { // transparent
					number_of_twos += 1
				}

			}
		}

		if min_number_of_zeros == -1 || number_of_zeros < min_number_of_zeros {
			min_number_of_zeros = number_of_zeros
			result = number_of_ones * number_of_twos
		}

	}

	fmt.Printf("Part 1: %d\n", result)
	fmt.Printf("Part 2:\n")

	for y := range height {
		for x := range width {
			switch image[x][y] {
			case 3:
				fmt.Printf(".")
			case 4:
				fmt.Printf("#")
			case 0:
				fmt.Printf("?")
			default:
				panic("Yikes!")
			}
		}
		fmt.Printf("\n")
	}
}
