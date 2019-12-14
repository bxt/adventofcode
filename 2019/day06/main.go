package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
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

func numberOfOrbitsInner(orbitMap map[string][]string, center string) (children int, orbits int) {
	trabants := orbitMap[center]

	for _, t := range trabants {
		trabantChildren, trabantOrbits := numberOfOrbitsInner(orbitMap, t)
		orbits += trabantOrbits
		children += trabantChildren
		children++
	}
	orbits += children

	return
}

func numberOfOrbits(orbiting []orbiting) int {
	comCandidates := make(map[string]bool)
	for _, o := range orbiting {
		comCandidates[o.center] = true
	}
	for _, o := range orbiting {
		comCandidates[o.trabant] = false
	}

	var com string
	for comCandidate, hasNoTrabants := range comCandidates {
		if hasNoTrabants {
			com = comCandidate
		}
	}

	orbitMap := make(map[string][]string)
	for _, o := range orbiting {
		trabants := orbitMap[o.center]
		trabants = append(trabants, o.trabant)
		orbitMap[o.center] = trabants
	}

	_, orbits := numberOfOrbitsInner(orbitMap, com)
	return orbits
}

type orbiting struct {
	center  string
	trabant string
}

func main() {
	// assertEquals(42, numberOfOrbits("COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"))

	file, err := os.Open("input.txt")
	check(err)
	defer file.Close()

	var orbitRelationships []orbiting

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		relationship := strings.Split(scanner.Text(), ")")
		orbitRelationships = append(orbitRelationships, orbiting{
			center:  relationship[0],
			trabant: relationship[1],
		})
	}
	check(scanner.Err())

	fmt.Printf("Part 1: %d\n", numberOfOrbits(orbitRelationships))
	// fmt.Printf("Part 2: %d\n", totalFuelRequirementWithTakeoffFuel)
}
