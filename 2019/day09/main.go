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

type machine struct {
	state          []int64
	input          []int64
	programCounter int64
	relativeBase   int64
	output         []int64
}

type mode int

const (
	positional mode = iota
	immediate
	relative
)

type operation int

const (
	noop operation = iota
	add
	mul
	in
	out
	jnz
	jz
	lt
	eq
	disp
	hlt operation = 99
)

func makeMachine(program []int64, input []int64) *machine {
	return &machine{
		state: append(program[:0:0], program...),
		input: input,
	}
}

func (m *machine) getRawOperand(position int64) int64 {
	return m.state[m.programCounter+position]
}

func (m *machine) getInstruction() int64 {
	return m.getRawOperand(0)
}

func (m *machine) getOperation() operation {
	return operation(m.getInstruction() % 100)
}

func (m *machine) getRawOperandMode(position int64) int64 {
	instruction := m.getInstruction()
	for range position + 1 {
		instruction /= 10
	}
	return instruction % 10
}

func (m *machine) getOperandMode(position int64) mode {
	return mode(m.getRawOperandMode(position))
}

func (m *machine) writeOperand(position int64, value int64) {
	address := m.getRawOperand(position)
	mode := m.getOperandMode(position)
	switch mode {
	case relative:
		address += m.relativeBase
		fallthrough
	case positional:
		if int64(len(m.state)) <= address {
			m.state = append(m.state, make([]int64, int(address)-len(m.state)+1)...)
		}
		m.state[address] = value
	default:
		panic(fmt.Sprintf("Unknown operand mode for write: %d", mode))
	}
}

func (m *machine) resolveOperand(position int64) int64 {
	address := m.getRawOperand(position)
	mode := m.getOperandMode(position)
	switch mode {
	case relative:
		address += m.relativeBase
		fallthrough
	case positional:
		if address >= int64(len(m.state)) {
			return 0
		}
		return m.state[address]
	case immediate:
		return address
	default:
		panic(fmt.Sprintf("Unknown operand mod for read: %d", mode))
	}
}

func (m *machine) emitOutput(value int64) {
	m.output = append(m.output, value)
}

func (m *machine) dequeueInput() int64 {
	value := m.input[0]
	m.input = m.input[1:]
	return value
}

func (m *machine) advanceProgramCounter(by int64) {
	m.programCounter += by
}

func (m *machine) jumpProgramCounter(to int64) {
	m.programCounter = to
}

func (m *machine) moveRelativeBase(by int64) {
	m.relativeBase += by
}

func (m *machine) run() {
Loop:
	for {
		operation := m.getOperation()
		// fmt.Printf("Op: %d, pc: %d, mA: %d, mB: %d, st: %v\n", operation, m.programCounter, m.getOperandMode(1), m.getOperandMode(2), m.state)
		switch operation {
		case add:
			m.writeOperand(3, m.resolveOperand(1)+m.resolveOperand(2))
			m.advanceProgramCounter(4)
		case mul:
			m.writeOperand(3, m.resolveOperand(1)*m.resolveOperand(2))
			m.advanceProgramCounter(4)
		case in:
			m.writeOperand(1, m.dequeueInput())
			m.advanceProgramCounter(2)
		case out:
			m.emitOutput(m.resolveOperand(1))
			m.advanceProgramCounter(2)
		case jnz:
			if m.resolveOperand(1) != 0 {
				m.jumpProgramCounter(m.resolveOperand(2))
			} else {
				m.advanceProgramCounter(3)
			}
		case jz:
			if m.resolveOperand(1) == 0 {
				m.jumpProgramCounter(m.resolveOperand(2))
			} else {
				m.advanceProgramCounter(3)
			}
		case lt:
			if m.resolveOperand(1) < m.resolveOperand(2) {
				m.writeOperand(3, 1)
			} else {
				m.writeOperand(3, 0)
			}
			m.advanceProgramCounter(4)
		case eq:
			if m.resolveOperand(1) == m.resolveOperand(2) {
				m.writeOperand(3, 1)
			} else {
				m.writeOperand(3, 0)
			}
			m.advanceProgramCounter(4)
		case disp:
			a := m.resolveOperand(1)
			m.moveRelativeBase(a)
			m.advanceProgramCounter(2)
		case hlt:
			break Loop
		default:
			panic(fmt.Sprintf("Unknown operation: %d", operation))
		}
	}
	// fmt.Printf("st: %v\n", state)
}

func runProgram(program []int64, input []int64) []int64 {
	m := makeMachine(program, input)
	m.run()
	return m.output
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

	programQuineExample := []int64{109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99}
	assertEquals(programQuineExample, runProgram(programQuineExample, []int64{}))

	programLargeNumberExample := []int64{1102, 34915192, 34915192, 7, 4, 7, 99, 0}
	assertEquals([]int64{1219070632396864}, runProgram(programLargeNumberExample, []int64{}))

	programMiddleExample := []int64{104, 1125899906842624, 99}
	assertEquals([]int64{1125899906842624}, runProgram(programMiddleExample, []int64{}))

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

	fmt.Printf("outputPart1: %v\n", outputPart1)

	for i, v := range outputPart1 {
		if i < len(outputPart1)-1 {
			if v != 0 {
				panic(fmt.Sprintf("Non-zero check %d at %d", v, i))
			}
		} else {
			fmt.Printf("Part 1: %d\n", v) // 3780860499
		}
	}

	inputPart2 := []int64{2}
	outputPart2 := runProgram(program, inputPart2)

	fmt.Printf("Part 2: %d\n", outputPart2[0])
}
