package main

import (
	"bufio"
	"fmt"
	"os"
	"reflect"
	"sort"
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

func numberOfChildrenAndOrbits(orbitMap map[string][]string, center string) (children int, orbits int) {
	trabants := orbitMap[center]

	for _, t := range trabants {
		trabantChildren, trabantOrbits := numberOfChildrenAndOrbits(orbitMap, t)
		orbits += trabantOrbits
		children += trabantChildren
		children++
	}
	orbits += children

	return
}

func findCenterOfMass(orbiting []orbiting) string {
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

	return com
}

func makeOrbitMap(orbiting []orbiting) map[string][]string {
	orbitMap := make(map[string][]string)
	for _, o := range orbiting {
		trabants := orbitMap[o.center]
		trabants = append(trabants, o.trabant)
		orbitMap[o.center] = trabants
	}
	for _, trabants := range orbitMap {
		sort.Strings(trabants)
	}
	return orbitMap
}

func contains(haystack []string, needle string) bool {
	insertIndex := sort.SearchStrings(haystack, needle)
	return insertIndex < len(haystack) && haystack[insertIndex] == needle
}

func pathToRoot(orbitMap map[string][]string, target string) []string {
	var path []string

searching:
	for {
		for center, trabants := range orbitMap {
			if contains(trabants, target) {
				path = append([]string{center}, path...)
				target = center
				continue searching
			}
		}
		break
	}

	return path
}

func distanceBetween(orbitMap map[string][]string, a string, b string) int {
	pathA := pathToRoot(orbitMap, a)
	pathB := pathToRoot(orbitMap, b)

	for {
		if len(pathB) == 0 || len(pathA) == 0 {
			break
		}
		if pathA[0] != pathB[0] {
			break
		}
		pathA = pathA[1:]
		pathB = pathB[1:]
	}

	return len(pathA) + len(pathB)
}

type orbiting struct {
	center  string
	trabant string
}

func main() {
	// assertEquals(42, numberOfOrbits("COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"))
	assertEquals([]string{"A", "B", "C"}, pathToRoot(map[string][]string{"A": {"B", "M"}, "M": {"L"}, "B": {"C", "N", "O", "P"}, "C": {"L", "Q", "X", "Y", "Z"}}, "X"))

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

	com := findCenterOfMass(orbitRelationships)
	orbitMap := makeOrbitMap(orbitRelationships)
	_, numberOfOrbits := numberOfChildrenAndOrbits(orbitMap, com)

	distance := distanceBetween(orbitMap, "SAN", "YOU")

	fmt.Printf("Part 1: %d\n", numberOfOrbits) // 402879
	fmt.Printf("Part 2: %d\n", distance)       // 484
}
