package main

import (
	"fmt"
	"io/ioutil"
	"reflect"
	"strconv"
	"strings"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func assertEquals(expected interface{}, actual interface{}) {
	if !reflect.DeepEqual(expected, actual) {
		panic(fmt.Sprintf(
			"expected = %[1]v : %[1]T != actual = %[2]v : %[2]T",
			expected, actual))
	}
}

type machine struct {
	state          []int
	input          []int
	programCounter int
	output         []int
}

type mode int

const (
	positional mode = iota
	immediate
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
	hlt operation = 99
)

func makeMachine(program []int, input []int) *machine {
	return &machine{
		state: append(program[:0:0], program...),
		input: input,
	}
}

func (m *machine) getRawOperand(position int) int {
	return m.state[m.programCounter+position]
}

func (m *machine) getInstruction() int {
	return m.getRawOperand(0)
}

func (m *machine) getOperation() operation {
	return operation(m.getInstruction() % 100)
}

func (m *machine) getRawOperandMode(position int) int {
	instruction := m.getInstruction()
	instruction /= 10
	for i := 0; i < position; i++ {
		instruction /= 10
	}
	return instruction % 10
}

func (m *machine) getOperandMode(position int) mode {
	return mode(m.getRawOperandMode(position))
}

func (m *machine) writeOperand(position int, value int) {
	m.state[m.getRawOperand(position)] = value
}

func (m *machine) resolveOperand(position int) int {
	value := m.getRawOperand(position)
	mode := m.getOperandMode(position)
	switch mode {
	case positional:
		return m.state[value]
	case immediate:
		return value
	default:
		panic(fmt.Sprintf("Unknown operand mode: %d", mode))
	}
}

func (m *machine) emitOutput(value int) {
	m.output = append(m.output, value)
}

func (m *machine) dequeueInput() int {
	value := m.input[0]
	m.input = m.input[1:]
	return value
}

func (m *machine) advanceProgramCounter(by int) {
	m.programCounter += by
}

func (m *machine) jumpProgramCounter(to int) {
	m.programCounter = to
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
		case hlt:
			break Loop
		default:
			panic(fmt.Sprintf("Unknown operation: %d", operation))
		}
	}
	// fmt.Printf("st: %v\n", state)
}

func runProgram(program []int, input []int) []int {
	m := makeMachine(program, input)
	m.run()
	return m.output
}

func main() {
	programEcho := []int{3, 0, 4, 0, 99}
	assertEquals([]int{42}, runProgram(programEcho, []int{42}))
	assertEquals([]int{64}, runProgram(programEcho, []int{64}))

	programImmediateEcho := []int{3, 3, 104, 0, 99}
	assertEquals([]int{42}, runProgram(programImmediateEcho, []int{42}))
	assertEquals([]int{64}, runProgram(programImmediateEcho, []int{64}))

	programDynamic := []int{3, 6, 3, 0, 3, 1, -1, 0, 1, 0, 4, 0, 99}
	assertEquals([]int{9}, runProgram(programDynamic, []int{1, 4, 5}))
	assertEquals([]int{20}, runProgram(programDynamic, []int{2, 4, 5}))

	assertEquals([]int{297}, runProgram([]int{1002, 6, 3, 7, 4, 7, 99, -1}, []int{}))
	assertEquals([]int{297}, runProgram([]int{102, 3, 6, 7, 4, 7, 99, -1}, []int{}))
	assertEquals([]int{102}, runProgram([]int{1001, 6, 3, 7, 4, 7, 99, -1}, []int{}))
	assertEquals([]int{102}, runProgram([]int{101, 3, 6, 7, 4, 7, 99, -1}, []int{}))

	dat, err := ioutil.ReadFile("../input.txt")
	check(err)

	programStrings := strings.Split(string(dat), ",")
	var program []int
	for _, s := range programStrings {
		i, err := strconv.Atoi(strings.TrimSpace(s))
		check(err)
		program = append(program, int(i))
	}

	inputPart1 := []int{1}
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

	programEqualPositionMode := []int{3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8}
	assertEquals([]int{0}, runProgram(programEqualPositionMode, []int{7}))
	assertEquals([]int{1}, runProgram(programEqualPositionMode, []int{8}))
	assertEquals([]int{0}, runProgram(programEqualPositionMode, []int{9}))

	programLessThanPositionMode := []int{3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8}
	assertEquals([]int{1}, runProgram(programLessThanPositionMode, []int{7}))
	assertEquals([]int{0}, runProgram(programLessThanPositionMode, []int{8}))
	assertEquals([]int{0}, runProgram(programLessThanPositionMode, []int{9}))

	programmEqualImmediateMode := []int{3, 3, 1108, -1, 8, 3, 4, 3, 99}
	assertEquals([]int{0}, runProgram(programmEqualImmediateMode, []int{7}))
	assertEquals([]int{1}, runProgram(programmEqualImmediateMode, []int{8}))
	assertEquals([]int{0}, runProgram(programmEqualImmediateMode, []int{9}))

	programmLessThanImmediateMode := []int{3, 3, 1107, -1, 8, 3, 4, 3, 99}
	assertEquals([]int{1}, runProgram(programmLessThanImmediateMode, []int{7}))
	assertEquals([]int{0}, runProgram(programmLessThanImmediateMode, []int{8}))
	assertEquals([]int{0}, runProgram(programmLessThanImmediateMode, []int{9}))

	programmJumpTestPositionMode := []int{3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9}
	assertEquals([]int{0}, runProgram(programmJumpTestPositionMode, []int{0}))
	assertEquals([]int{1}, runProgram(programmJumpTestPositionMode, []int{3}))
	assertEquals([]int{1}, runProgram(programmJumpTestPositionMode, []int{-2}))

	programmJumpTestImmediateMode := []int{3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1}
	assertEquals([]int{0}, runProgram(programmJumpTestImmediateMode, []int{0}))
	assertEquals([]int{1}, runProgram(programmJumpTestImmediateMode, []int{3}))
	assertEquals([]int{1}, runProgram(programmJumpTestImmediateMode, []int{-2}))

	programmLargerExample := []int{3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99}
	assertEquals([]int{999}, runProgram(programmLargerExample, []int{7}))
	assertEquals([]int{1000}, runProgram(programmLargerExample, []int{8}))
	assertEquals([]int{1001}, runProgram(programmLargerExample, []int{9}))

	inputPart2 := []int{5}
	outputPart2 := runProgram(program, inputPart2)

	fmt.Printf("Part 2: %d\n", outputPart2[0]) // 11826654
}
