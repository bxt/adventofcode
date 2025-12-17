package main

import (
	"bufio"
	"cmp"
	"errors"
	"fmt"
	"io"
	"maps"
	"math"
	"os"
	"reflect"
	"slices"
	"time"
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

type coord struct {
	x int
	y int
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

func equals(a, b coord) bool {
	return a.x == b.x && a.y == b.y
}

func minus(a, b coord) coord {
	return coord{x: a.x - b.x, y: a.y - b.y}
}

func divide(a coord, divisor int) coord {
	return coord{x: a.x / divisor, y: a.y / divisor}
}

func (c coord) directionFrom(to coord) coord {
	d := minus(c, to)
	divisor := gcd(abs(d.x), abs(d.y))
	return divide(d, divisor)
}

func (c coord) manhattanMagnitude() int {
	return abs(c.x) + abs(c.y)
}

func (c coord) angle() float64 {
	return -math.Atan2(float64(c.x), float64(c.y))
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

	var bestVisible int
	var bestCoord coord

	for baseY, line := range grid {
		for baseX, base := range line {
			if base {
				baseCoord := coord{x: baseX, y: baseY}
				visible := 0
				for asteroidY, asteroidLine := range grid {
				asteroid_checking:
					for asteroidX, asteroid := range asteroidLine {
						asteroidCoord := coord{x: asteroidX, y: asteroidY}
						if asteroid && !equals(asteroidCoord, baseCoord) {
							direction := asteroidCoord.directionFrom(baseCoord)

							for x, y := baseX+direction.x, baseY+direction.y; x != asteroidX || y != asteroidY; x, y = x+direction.x, y+direction.y {
								if grid[y][x] {
									continue asteroid_checking
								}
							}

							visible++
						}
					}
				}

				if visible > bestVisible {
					bestVisible = visible
					bestCoord = baseCoord
				}
			}
		}
	}

	fmt.Printf("Part 1: %d\n", bestVisible) // 288

	asteroids := make(map[coord][]coord)

	for asteroidY, asteroidLine := range grid {
		for asteroidX, asteroid := range asteroidLine {
			asteroidCoord := coord{x: asteroidX, y: asteroidY}

			if asteroid && !equals(asteroidCoord, bestCoord) {
				direction := asteroidCoord.directionFrom(bestCoord)

				asteroids[direction] = append(asteroids[direction], coord{x: asteroidX, y: asteroidY})
			}
		}
	}

	directions := slices.Collect(maps.Keys(asteroids))

	slices.SortFunc(directions, func(a, b coord) int {
		return cmp.Compare(a.angle(), b.angle())
	})

	for _, direction := range directions {
		slices.SortFunc(asteroids[direction], func(a, b coord) int {
			dA := minus(a, bestCoord)
			dB := minus(b, bestCoord)
			return cmp.Compare(dA.manhattanMagnitude(), dB.manhattanMagnitude())
		})
	}

	vaporizedCount := 0
vaporizing:
	for {
		for _, direction := range directions {
			asteroidsInDirection := asteroids[direction]
			if len(asteroidsInDirection) > 0 {
				vaporizedAsteroid := asteroidsInDirection[0]
				asteroids[direction] = asteroidsInDirection[1:]
				grid[vaporizedAsteroid.y][vaporizedAsteroid.x] = false
				vaporizedCount++

				fmt.Print("\033[H\033[2J")
				fmt.Printf("Part 1: %d\n\n", bestVisible)

				for y, line := range grid {
					for x, cell := range line {
						coord := coord{x: x, y: y}
						if equals(coord, vaporizedAsteroid) {
							fmt.Print("\033[31m#\033[0m")
						} else if equals(coord, bestCoord) {
							fmt.Print("\033[32mX\033[0m")
						} else if cell {
							fmt.Print("#")
						} else {
							fmt.Print(".")
						}
					}
					fmt.Println()
				}

				fmt.Printf("\nVaporized %d: \033[31m(%d,%d)\033[0m\n", vaporizedCount, vaporizedAsteroid.x, vaporizedAsteroid.y)

				if vaporizedCount == 200 {
					result := vaporizedAsteroid.x*100 + vaporizedAsteroid.y
					fmt.Printf("Part 2: %d\n", result) // 616
					break vaporizing
				}

				time.Sleep(100 * time.Millisecond)
			}
		}
	}
}
