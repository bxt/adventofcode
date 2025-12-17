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
	var bestX int
	var bestY int

	for baseY, line := range grid {
		for baseX, base := range line {
			if base {
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

				if visible > bestVisible {
					bestVisible = visible
					bestX = baseX
					bestY = baseY
				}
			}
		}
	}

	fmt.Printf("Part 1: %d\n", bestVisible) // 288

	asteroids := make(map[coord][]coord)

	for asteroidY, asteroidLine := range grid {
		for asteroidX, asteroid := range asteroidLine {
			if asteroid && !(asteroidX == bestX && asteroidY == bestY) {
				dx := asteroidX - bestX
				dy := asteroidY - bestY

				divisor := gcd(abs(dx), abs(dy))

				xStep := dx / divisor
				yStep := dy / divisor

				direction := coord{x: xStep, y: yStep}

				asteroids[direction] = append(asteroids[direction], coord{x: asteroidX, y: asteroidY})
			}
		}
	}

	directions := slices.Collect(maps.Keys(asteroids))

	slices.SortFunc(directions, func(a, b coord) int {
		da := math.Atan2(float64(a.x), float64(a.y))
		db := math.Atan2(float64(b.x), float64(b.y))
		return cmp.Compare(db, da)
	})

	for _, direction := range directions {
		slices.SortFunc(asteroids[direction], func(a, b coord) int {
			dxA := a.x - bestX
			dyA := a.y - bestY
			dxB := b.x - bestX
			dyB := b.y - bestY
			distA := abs(dxA) + abs(dyA)
			distB := abs(dxB) + abs(dyB)
			return cmp.Compare(distA, distB)
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
						if x == vaporizedAsteroid.x && y == vaporizedAsteroid.y {
							fmt.Print("\033[31m#\033[0m")
						} else if x == bestX && y == bestY {
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
