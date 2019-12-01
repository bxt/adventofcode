package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func assertEquals(expected interface{}, actual interface{}) {
	if expected != actual {
		panic(fmt.Sprintf(
			"expected = %[1]v : %[1]T != actual = %[2]v : %[2]T",
			expected, actual))
	}
}

func fuelRequirement(weight int) int {
	return weight/3 - 2
}

func fuelRequirementWithTakeoffFuel(weight int) int {
	totalFuel := 0
	for fuel := weight; ; {
		fuel = fuelRequirement(fuel)
		if fuel <= 0 {
			break
		}
		totalFuel += fuel
	}
	return totalFuel
}

func main() {
	assertEquals(2, fuelRequirement(12))
	assertEquals(2, fuelRequirement(14))
	assertEquals(654, fuelRequirement(1969))
	assertEquals(33583, fuelRequirement(100756))

	assertEquals(2, fuelRequirementWithTakeoffFuel(14))
	assertEquals(966, fuelRequirementWithTakeoffFuel(1969))
	assertEquals(50346, fuelRequirementWithTakeoffFuel(100756))

	file, err := os.Open("input.txt")
	check(err)
	defer file.Close()

	totalFuelRequirement := 0
	totalFuelRequirementWithTakeoffFuel := 0

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		mass, err := strconv.Atoi(scanner.Text())
		check(err)
		totalFuelRequirement += fuelRequirement(mass)
		totalFuelRequirementWithTakeoffFuel += fuelRequirementWithTakeoffFuel(mass)
	}
	check(scanner.Err())

	fmt.Printf("Part 1: %d\n", totalFuelRequirement)
	fmt.Printf("Part 2: %d\n", totalFuelRequirementWithTakeoffFuel)
}
