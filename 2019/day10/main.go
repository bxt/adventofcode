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

func abs(n int) int {
	if n < 0 {
		return -n
	}
	return n
}

func gcd(a int, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

func main() {
	var grid [][]bool

	file, err := os.Open("input.txt")
	check(err)
	defer file.Close()

	reader := bufio.NewReader(file)

	var line []bool

reading:
	for {
		b, err := reader.ReadByte()
		if errors.Is(err, io.EOF) {
			break reading
		} else {
			check(err)
		}

		if b == '.' {
			line = append(line, false)
		}
		if b == '#' {
			line = append(line, true)
		}
		if b == '\n' {
			grid = append(grid, line)
			line = nil
		}

	}

	var best int

	for baseY, line := range grid {
		for baseX, base := range line {
			if base {
				// check how many other asteroids are visible from here
				visible := 0
				for asteroidY, asteroidLine := range grid {
				asteroid_checking:
					for asteroidX, asteroid := range asteroidLine {
						if asteroid && !(asteroidX == baseX && asteroidY == baseY) {
							dx := asteroidX - baseX
							dy := asteroidY - baseY

							divisor := gcd(abs(dx), abs(dy))

							xStep := dx / divisor
							yStep := dy / divisor

							for x, y := baseX+xStep, baseY+yStep; x != asteroidX || y != asteroidY; x, y = x+xStep, y+yStep {
								if grid[y][x] {
									continue asteroid_checking
								}
							}

							visible++
						}
					}
				}

				if visible > best {
					best = visible
				}
			}
		}
	}

	fmt.Printf("Part 1: %d\n", best)
}
