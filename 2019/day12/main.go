package main

import (
	"fmt"
	"os"
	"reflect"
	"strings"
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

type coord3 struct {
	x int
	y int
	z int
}

func add(a, b coord3) coord3 {
	return coord3{x: a.x + b.x, y: a.y + b.y, z: a.z + b.z}
}

func minus(a, b coord3) coord3 {
	return coord3{x: a.x - b.x, y: a.y - b.y, z: a.z - b.z}
}

func sign(i int) int {
	if i < 0 {
		return -1
	} else if i > 0 {
		return 1
	} else {
		return 0
	}
}

func (c coord3) sign() coord3 {
	return coord3{x: sign(c.x), y: sign(c.y), z: sign(c.z)}
}

func abs(n int) int {
	if n < 0 {
		return -n
	}
	return n
}

func (c coord3) manhattanMagnitude() int {
	return abs(c.x) + abs(c.y) + abs(c.z)
}

func main() {
	dat, err := os.ReadFile("input.txt")
	check(err)
	lines := strings.Split(strings.TrimSpace(string(dat)), "\n")

	var positions []coord3
	for _, line := range lines {
		var x, y, z int
		_, err := fmt.Sscanf(line, "<x=%d, y=%d, z=%d>", &x, &y, &z)
		check(err)
		positions = append(positions, coord3{x: x, y: y, z: z})
	}

	velocities := make([]coord3, len(positions))
	for range 1000 {
		// Apply gravity
		for i, position := range positions {
			for _, other := range positions {
				delta := minus(other, position).sign()
				velocities[i] = add(velocities[i], delta)
			}
		}
		// Apply velocity
		for i, _ := range positions {
			positions[i] = add(positions[i], velocities[i])
		}
	}

	totalEnergy := 0
	for i, _ := range positions {
		potentialEnergy := positions[i].manhattanMagnitude()
		kineticEnergy := velocities[i].manhattanMagnitude()
		totalEnergy += potentialEnergy * kineticEnergy
	}

	fmt.Printf("Part 1: %d\n", totalEnergy) // 125036
}
