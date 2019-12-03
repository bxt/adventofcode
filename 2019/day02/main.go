package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
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

func runProgram(state []int64, resultPointer int64) int64 {
	pc := 0
	for opCode := state[pc]; opCode != 99; opCode = state[pc] {
		// fmt.Printf("Op: %d, pc: %d\n", opCode, pc)
		if opCode == 1 {
			operandAPointer, operandBPointer, rPointer := state[pc+1], state[pc+2], state[pc+3]
			// fmt.Printf("%d(%d) = %d (%d) + %d (%d)\n", state[operandAPointer]+state[operandBPointer], rPointer, state[operandAPointer], operandAPointer, state[operandBPointer], operandBPointer)
			state[rPointer] = state[operandAPointer] + state[operandBPointer]
			pc += 4
		} else if opCode == 2 {
			operandAPointer, operandBPointer, rPointer := state[pc+1], state[pc+2], state[pc+3]
			// fmt.Printf("%d(%d) = %d (%d) * %d (%d)\n", state[operandAPointer]*state[operandBPointer], rPointer, state[operandAPointer], operandAPointer, state[operandBPointer], operandBPointer)
			state[rPointer] = state[operandAPointer] * state[operandBPointer]
			pc += 4
		} else {
			panic(fmt.Sprintf("Unknown op code: %d", opCode))
		}
		// fmt.Printf("St: %v, npc: %d, nop: %v\n", state, pc, state[pc])
	}
	return state[resultPointer]
}

func main() {
	assertEquals(int64(3500), runProgram([]int64{1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50}, 0))
	assertEquals(int64(2), runProgram([]int64{1, 0, 0, 0, 99}, 0))
	assertEquals(int64(6), runProgram([]int64{2, 3, 0, 3, 99}, 3))
	assertEquals(int64(9801), runProgram([]int64{2, 4, 4, 5, 99, 0}, 5))
	assertEquals(int64(30), runProgram([]int64{1, 1, 1, 4, 99, 5, 6, 0, 99}, 0))

	dat, err := ioutil.ReadFile("input.txt")
	check(err)

	programStrings := strings.Split(string(dat), ",")
	var program []int64
	for _, s := range programStrings {
		i, err := strconv.Atoi(strings.TrimSpace(s))
		check(err)
		program = append(program, int64(i))
	}

	programmWithNounAndVerb := append(program[:0:0], program...)

	programmWithNounAndVerb[1] = 12
	programmWithNounAndVerb[2] = 2

	part1 := runProgram(programmWithNounAndVerb, 0)

	fmt.Printf("Part 1: %d\n", part1)

	for noun := int64(0); noun < 100; noun++ {
		for verb := int64(0); verb < 100; verb++ {
			programmWithNounAndVerb := append(program[:0:0], program...)

			programmWithNounAndVerb[1] = noun
			programmWithNounAndVerb[2] = verb

			result := runProgram(programmWithNounAndVerb, 0)

			if result == 19690720 {
				fmt.Printf("Part 2: %d\n", 100*noun+verb)
			}
		}
	}

}
