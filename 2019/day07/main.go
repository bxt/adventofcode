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

func maxThrusterSignal(program []int64) (topValue int64, topSequence []int64) {
	topValue = int64(0)
	for a := range int64(5) {
		for b := range int64(5) {
			if b == a {
				continue
			}
			for c := range int64(5) {
				if c == a {
					continue
				}
				if c == b {
					continue
				}
				for d := range int64(5) {
					if d == a {
						continue
					}
					if d == b {
						continue
					}
					if d == c {
						continue
					}
					for e := range int64(5) {
						if e == a {
							continue
						}
						if e == b {
							continue
						}
						if e == c {
							continue
						}
						if e == d {
							continue
						}
						valA := runProgram(program, []int64{a, 0})[0]
						valB := runProgram(program, []int64{b, valA})[0]
						valC := runProgram(program, []int64{c, valB})[0]
						valD := runProgram(program, []int64{d, valC})[0]
						valE := runProgram(program, []int64{e, valD})[0]
						// fmt.Printf("got %v,", valE)
						if valE > topValue {
							topValue = valE
							topSequence = []int64{a, b, c, d, e}
							// fmt.Printf("New top: %v with %d\n", topValue, topSequence)
						}
					}
				}
			}
		}
	}
	return
}

func main() {
	topValue, topSequence := maxThrusterSignal([]int64{3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0})
	assertEquals([]int64{4, 3, 2, 1, 0}, topSequence)
	assertEquals(int64(43210), topValue)

	topValue, topSequence = maxThrusterSignal([]int64{3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0})
	assertEquals([]int64{0, 1, 2, 3, 4}, topSequence)
	assertEquals(int64(54321), topValue)

	topValue, topSequence = maxThrusterSignal([]int64{3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33, 1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0})
	assertEquals([]int64{1, 0, 4, 3, 2}, topSequence)
	assertEquals(int64(65210), topValue)

	dat, err := os.ReadFile("input.txt")
	check(err)

	programStrings := strings.Split(string(dat), ",")
	var program []int64
	for _, s := range programStrings {
		i, err := strconv.Atoi(strings.TrimSpace(s))
		check(err)
		program = append(program, int64(i))
	}

	topValue, topSequence = maxThrusterSignal(program)
	fmt.Printf("Part 1: %v\n", topValue)
}
