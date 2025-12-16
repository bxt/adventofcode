package main

import (
	"fmt"
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

func decodeInstruction(instruction int64) (opCode, modeOperandA, modeOperandB, modeOperandC int64) {
	opCode = instruction % 100
	instruction /= 100
	modeOperandA = instruction % 10
	instruction /= 10
	modeOperandB = instruction % 10
	instruction /= 10
	modeOperandC = instruction % 10
	return
}

func resolveOperand(state []int64, value int64, mode int64) int64 {
	switch mode {
	case 0:
		return state[value]
	case 1:
		return value
	default:
		panic(fmt.Sprintf("Unknown operand mode: %d", mode))
	}
}

func runProgram(program []int64, input []int64) []int64 {
	state := append(program[:0:0], program...)
	pc := 0
	var output []int64

Loop:
	for {
		opCode, modeOperandA, modeOperandB, _ :=
			decodeInstruction(state[pc])
		// fmt.Printf("Op: %d, pc: %d, mA: %d, mB: %d, st: %v\n", opCode, pc, modeOperandA, modeOperandB, state)
		switch opCode {
		case 1:
			operandA, operandB := state[pc+1], state[pc+2]
			rPointer := state[pc+3]
			a := resolveOperand(state, operandA, modeOperandA)
			b := resolveOperand(state, operandB, modeOperandB)
			state[rPointer] = a + b
			pc += 4
		case 2:
			operandA, operandB := state[pc+1], state[pc+2]
			rPointer := state[pc+3]
			a := resolveOperand(state, operandA, modeOperandA)
			b := resolveOperand(state, operandB, modeOperandB)
			state[rPointer] = a * b
			pc += 4
		case 3:
			rPointer := state[pc+1]
			a := input[0]
			input = input[1:]
			state[rPointer] = a
			pc += 2
		case 4:
			operandA := state[pc+1]
			a := resolveOperand(state, operandA, modeOperandA)
			output = append(output, a)
			pc += 2
		case 5:
			operandA, operandB := state[pc+1], state[pc+2]
			a := resolveOperand(state, operandA, modeOperandA)
			if a != 0 {
				b := resolveOperand(state, operandB, modeOperandB)
				pc = int(b)
			} else {
				pc += 3
			}
		case 6:
			operandA, operandB := state[pc+1], state[pc+2]
			a := resolveOperand(state, operandA, modeOperandA)
			if a == 0 {
				b := resolveOperand(state, operandB, modeOperandB)
				pc = int(b)
			} else {
				pc += 3
			}
		case 7:
			operandA, operandB := state[pc+1], state[pc+2]
			rPointer := state[pc+3]
			a := resolveOperand(state, operandA, modeOperandA)
			b := resolveOperand(state, operandB, modeOperandB)
			if a < b {
				state[rPointer] = 1
			} else {
				state[rPointer] = 0
			}
			pc += 4
		case 8:
			operandA, operandB := state[pc+1], state[pc+2]
			rPointer := state[pc+3]
			a := resolveOperand(state, operandA, modeOperandA)
			b := resolveOperand(state, operandB, modeOperandB)
			if a == b {
				state[rPointer] = 1
			} else {
				state[rPointer] = 0
			}
			pc += 4
		case 99:
			break Loop
		default:
			panic(fmt.Sprintf("Unknown op code: %d", opCode))
		}
	}
	// fmt.Printf("st: %v\n", state)
	return output
}

func main() {
	programEcho := []int64{3, 0, 4, 0, 99}
	assertEquals([]int64{42}, runProgram(programEcho, []int64{42}))
	assertEquals([]int64{64}, runProgram(programEcho, []int64{64}))

	programImmediateEcho := []int64{3, 3, 104, 0, 99}
	assertEquals([]int64{42}, runProgram(programImmediateEcho, []int64{42}))
	assertEquals([]int64{64}, runProgram(programImmediateEcho, []int64{64}))

	programDynamic := []int64{3, 6, 3, 0, 3, 1, -1, 0, 1, 0, 4, 0, 99}
	assertEquals([]int64{9}, runProgram(programDynamic, []int64{1, 4, 5}))
	assertEquals([]int64{20}, runProgram(programDynamic, []int64{2, 4, 5}))

	assertEquals([]int64{297}, runProgram([]int64{1002, 6, 3, 7, 4, 7, 99, -1}, []int64{}))
	assertEquals([]int64{297}, runProgram([]int64{102, 3, 6, 7, 4, 7, 99, -1}, []int64{}))
	assertEquals([]int64{102}, runProgram([]int64{1001, 6, 3, 7, 4, 7, 99, -1}, []int64{}))
	assertEquals([]int64{102}, runProgram([]int64{101, 3, 6, 7, 4, 7, 99, -1}, []int64{}))

	dat, err := os.ReadFile("input.txt")
	check(err)

	programStrings := strings.Split(string(dat), ",")
	var program []int64
	for _, s := range programStrings {
		i, err := strconv.Atoi(strings.TrimSpace(s))
		check(err)
		program = append(program, int64(i))
	}

	inputPart1 := []int64{1}
	outputPart1 := runProgram(program, inputPart1)

	for i, v := range outputPart1 {
		if i < len(outputPart1)-1 {
			if v != 0 {
				panic(fmt.Sprintf("Non-zero check %d at %d", v, i))
			}
		} else {
			fmt.Printf("Part 1: %d\n", v) // 7259358
		}
	}

	programEqualPositionMode := []int64{3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8}
	assertEquals([]int64{0}, runProgram(programEqualPositionMode, []int64{7}))
	assertEquals([]int64{1}, runProgram(programEqualPositionMode, []int64{8}))
	assertEquals([]int64{0}, runProgram(programEqualPositionMode, []int64{9}))

	programLessThanPositionMode := []int64{3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8}
	assertEquals([]int64{1}, runProgram(programLessThanPositionMode, []int64{7}))
	assertEquals([]int64{0}, runProgram(programLessThanPositionMode, []int64{8}))
	assertEquals([]int64{0}, runProgram(programLessThanPositionMode, []int64{9}))

	programEqualImmediateMode := []int64{3, 3, 1108, -1, 8, 3, 4, 3, 99}
	assertEquals([]int64{0}, runProgram(programEqualImmediateMode, []int64{7}))
	assertEquals([]int64{1}, runProgram(programEqualImmediateMode, []int64{8}))
	assertEquals([]int64{0}, runProgram(programEqualImmediateMode, []int64{9}))

	programLessThanImmediateMode := []int64{3, 3, 1107, -1, 8, 3, 4, 3, 99}
	assertEquals([]int64{1}, runProgram(programLessThanImmediateMode, []int64{7}))
	assertEquals([]int64{0}, runProgram(programLessThanImmediateMode, []int64{8}))
	assertEquals([]int64{0}, runProgram(programLessThanImmediateMode, []int64{9}))

	programJumpTestPositionMode := []int64{3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9}
	assertEquals([]int64{0}, runProgram(programJumpTestPositionMode, []int64{0}))
	assertEquals([]int64{1}, runProgram(programJumpTestPositionMode, []int64{3}))
	assertEquals([]int64{1}, runProgram(programJumpTestPositionMode, []int64{-2}))

	programJumpTestImmediateMode := []int64{3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1}
	assertEquals([]int64{0}, runProgram(programJumpTestImmediateMode, []int64{0}))
	assertEquals([]int64{1}, runProgram(programJumpTestImmediateMode, []int64{3}))
	assertEquals([]int64{1}, runProgram(programJumpTestImmediateMode, []int64{-2}))

	programLargerExample := []int64{3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99}
	assertEquals([]int64{999}, runProgram(programLargerExample, []int64{7}))
	assertEquals([]int64{1000}, runProgram(programLargerExample, []int64{8}))
	assertEquals([]int64{1001}, runProgram(programLargerExample, []int64{9}))

	inputPart2 := []int64{5}
	outputPart2 := runProgram(program, inputPart2)

	fmt.Printf("Part 2: %d\n", outputPart2[0]) // 11826654
}
