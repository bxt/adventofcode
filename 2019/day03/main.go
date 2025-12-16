package main

import (
	"bufio"
	"fmt"
	"image"
	"image/color"
	"image/png"
	"os"
	"reflect"
	"strconv"
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

type instruction struct {
	direction string
	length    int
}

type line struct {
	x1 int
	y1 int
	x2 int
	y2 int
}

type point struct {
	x int
	y int
}

func parseInstructions(instructionList string) []instruction {
	var instructions []instruction
	for piece := range strings.SplitSeq(instructionList, ",") {
		length, err := strconv.Atoi(piece[1:])
		check(err)
		instructions = append(instructions, instruction{
			direction: piece[0:1],
			length:    length,
		})
	}
	return instructions
}

func convertInstructionsToLines(instructions []instruction) []line {
	var lines []line
	var x, y int
	for _, instruction := range instructions {
		oldX, oldY := x, y
		switch instruction.direction {
		case "U":
			y += instruction.length
		case "R":
			x += instruction.length
		case "L":
			x -= instruction.length
		case "D":
			y -= instruction.length
		}
		lines = append(lines, line{
			x1: oldX,
			y1: oldY,
			x2: x,
			y2: y,
		})
	}
	return lines
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func (p point) mag() int {
	return abs(p.x) + abs(p.y)
}

func minmax(a, b int) (int, int) {
	if a < b {
		return a, b
	}
	return b, a
}

func overlaps(from1, to1, from2, to2 int) (int, int) {
	from1, to1 = minmax(from1, to1)
	from2, to2 = minmax(from2, to2)
	if from1 <= from2 {
		if from2 <= to1 {
			to, _ := minmax(to1, to2)
			return from2, to
		}
		return -1, -2
	}

	if from1 <= to2 {
		to, _ := minmax(to1, to2)
		return from1, to
	}
	return -1, -2
}

func intersectionPoints(a line, b line) []point {
	if a.x1 == a.x2 { // a vertical
		if b.x1 == b.x2 { // b vertical
			if a.x1 == b.x1 {
				var intersections []point
				fromY, toY := overlaps(a.y1, a.y2, b.y1, b.y2)
				for y := fromY; y <= toY; y++ {
					intersections = append(intersections, point{a.x1, y})
				}
				fmt.Printf("%v and %v: X = %d, Y = from %d to %d\n", a, b, a.x1, fromY, toY)
				return intersections
			}
			return nil
		} else if b.y1 == b.y2 { // b horizontal
			bFrom, bTo := minmax(b.x1, b.x2)
			aFrom, aTo := minmax(a.y1, a.y2)
			if aFrom <= b.y1 && b.y1 <= aTo && bFrom <= a.x1 && a.x1 <= bTo {
				return []point{{a.x1, b.y1}}
			}
			return nil
		} else {
			panic("line b is not orthogonal")
		}
	} else if a.y1 == a.y2 { // a horizontal
		if b.x1 == b.x2 { // b vertical
			bFrom, bTo := minmax(b.y1, b.y2)
			aFrom, aTo := minmax(a.x1, a.x2)
			if aFrom <= b.x1 && b.x1 <= aTo && bFrom <= a.y1 && a.y1 <= bTo {
				return []point{{b.x1, a.y1}}
			}
			return nil
		} else if b.y1 == b.y2 { // b horizontal
			if a.y1 == b.y1 {
				var intersections []point
				fromX, toX := overlaps(a.x1, a.x2, b.x1, b.x2)
				for x := fromX; x <= toX; x++ {
					intersections = append(intersections, point{x, a.y1})
				}
				return intersections
			}
			return nil
		} else {
			panic("line b is not orthogonal")
		}

	} else {
		panic("line a is not orthogonal")
	}
}

func closestPoint(points []point) point {
	var closest point
	for i, point := range points {
		if i == 0 || point.mag() < closest.mag() {
			closest = point
		}
	}

	return closest
}

func visualizeLinesAndIntersections(wireLines [][]line, intersections []point) {
	var minX, maxX, minY, maxY int
	for _, lines := range wireLines {
		for _, line := range lines {
			minX, _ = minmax(minX, line.x1)
			minX, _ = minmax(minX, line.x2)
			_, maxX = minmax(maxX, line.x1)
			_, maxX = minmax(maxX, line.x2)
			minY, _ = minmax(minY, line.y1)
			minY, _ = minmax(minY, line.y2)
			_, maxY = minmax(maxY, line.y1)
			_, maxY = minmax(maxY, line.y2)
		}
	}

	pad := 10
	img := image.NewRGBA(image.Rectangle{
		image.Point{minX - pad, minY - pad},
		image.Point{maxX + pad, maxY + pad},
	})

	colors := []color.RGBA{
		{100, 200, 200, 0xff},
		{200, 200, 100, 0xff},
	}

	for wireIndex, lines := range wireLines {
		for _, line := range lines {
			if line.x1 == line.x2 {
				fy, ty := minmax(line.y1, line.y2)
				for y := fy; y <= ty; y++ {
					img.Set(line.x1, y, colors[wireIndex])
				}
			} else if line.y1 == line.y2 {
				fx, tx := minmax(line.x1, line.x2)
				for x := fx; x <= tx; x++ {
					img.Set(x, line.y1, colors[wireIndex])
				}
			} else {
				panic("line is not orthogonal")
			}
		}
	}

	blue := color.RGBA{0, 0, 255, 0xff}
	for _, point := range intersections {
		img.Set(point.x, point.y, blue)
	}

	red := color.RGBA{255, 0, 0, 0xff}
	img.Set(0, 0, red)

	f, _ := os.Create("image.png")
	png.Encode(f, img)
}

func wiresIntersectionPoints(wires [][]instruction) []point {
	var wireLines [][]line

	for _, instructions := range wires {
		wireLines = append(wireLines, convertInstructionsToLines(instructions))
	}

	linesA, linesB := wireLines[0], wireLines[1]

	var intersections []point

	for _, lineA := range linesA {
		for _, lineB := range linesB {
			intersections = append(intersections, intersectionPoints(lineA, lineB)...)
		}
	}

	var filteredIntersections []point
	for _, point := range intersections {
		if point.mag() > 0 {
			filteredIntersections = append(filteredIntersections, point)
		}
	}

	// visualizeLinesAndIntersections(wireLines, filteredIntersections)

	return filteredIntersections
}

func (line line) length() int {
	fx, tx := minmax(line.x1, line.x2)
	fy, ty := minmax(line.y1, line.y2)
	return ty - fy + tx - fx
}

func wiresOptimalIntersectionDistance(wires [][]instruction) int {
	var wireLines [][]line

	for _, instructions := range wires {
		wireLines = append(wireLines, convertInstructionsToLines(instructions))
	}

	linesA, linesB := wireLines[0], wireLines[1]

	minDistance := -1

	var distanceA int
	for _, lineA := range linesA {
		var distanceB int
		for _, lineB := range linesB {
			for _, intersection := range intersectionPoints(lineA, lineB) {
				if intersection.mag() > 0 {
					lineDistanceA := abs(intersection.x-lineA.x1) + abs(intersection.y-lineA.y1)
					lineDistanceB := abs(intersection.x-lineB.x1) + abs(intersection.y-lineB.y1)
					potentialDistance := distanceA + lineDistanceA + distanceB + lineDistanceB
					if minDistance == -1 || potentialDistance < minDistance {
						minDistance = potentialDistance
					}
				}
			}
			distanceB += lineB.length()
		}
		distanceA += lineA.length()
	}

	return minDistance
}

func main() {
	assertEquals([]instruction{{"U", 3}, {"L", 5}}, parseInstructions("U3,L5"))
	assertEquals([]line{{0, 0, 0, 3}, {0, 3, -5, 3}, {-5, 3, -5, 1}, {-5, 1, 4, 1}},
		convertInstructionsToLines(parseInstructions("U3,L5,D2,R9")))
	assertEquals(point{3, 3}, closestPoint(wiresIntersectionPoints([][]instruction{
		parseInstructions("R8,U5,L5,D3"), parseInstructions("U7,R6,D4,L4")})))
	assertEquals(159, closestPoint(wiresIntersectionPoints([][]instruction{
		parseInstructions("R75,D30,R83,U83,L12,D49,R71,U7,L72"),
		parseInstructions("U62,R66,U55,R34,D71,R55,D58,R83"),
	})).mag())
	assertEquals(135, closestPoint(wiresIntersectionPoints([][]instruction{
		parseInstructions("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"),
		parseInstructions("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"),
	})).mag())

	file, err := os.Open("input.txt")
	check(err)
	defer file.Close()

	var wires [][]instruction

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		wires = append(wires, parseInstructions(scanner.Text()))
	}
	check(scanner.Err())

	fmt.Printf("Part 1: %d\n", closestPoint(wiresIntersectionPoints(wires)).mag()) // 316

	assertEquals(30, wiresOptimalIntersectionDistance([][]instruction{
		parseInstructions("R8,U5,L5,D3"), parseInstructions("U7,R6,D4,L4")}))
	assertEquals(610, wiresOptimalIntersectionDistance([][]instruction{
		parseInstructions("R75,D30,R83,U83,L12,D49,R71,U7,L72"),
		parseInstructions("U62,R66,U55,R34,D71,R55,D58,R83")},
	))
	assertEquals(410, wiresOptimalIntersectionDistance([][]instruction{
		parseInstructions("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"),
		parseInstructions("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"),
	}))

	fmt.Printf("Part 2: %d\n", wiresOptimalIntersectionDistance(wires)) // ?
}
