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

type task int

const (
	x task = iota
	y
	paint
	taskCount
)

type machine struct {
	state           []int64
	programCounter  int64
	relativeBase    int64
	task            task
	currentPosition coord
	tileColors      map[coord]int64
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

func makeMachine(program []int64) *machine {
	return &machine{
		state:           append(program[:0:0], program...),
		task:            x,
		currentPosition: coord{x: 0, y: 0},
		tileColors:      make(map[coord]int64),
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
	switch m.task {
	case paint:
		m.tileColors[m.currentPosition] = value
	case x:
		m.currentPosition = coord{x: int(value), y: m.currentPosition.y}
	case y:
		m.currentPosition = coord{x: m.currentPosition.x, y: int(value)}
	default:
		panic(fmt.Sprintf("Unknown task: %d", m.task))
	}
	m.task = (m.task + 1) % taskCount
}

func (m *machine) dequeueInput() int64 {
	return m.tileColors[m.currentPosition]
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

type coord struct {
	x int
	y int
}

func runProgram(program []int64) map[coord]int64 {
	m := makeMachine(program)
	m.run()
	return m.tileColors
}

func main() {
	dat, err := os.ReadFile("input.txt")
	check(err)

	programStrings := strings.Split(string(dat), ",")
	var program []int64
	for _, s := range programStrings {
		i, err := strconv.Atoi(strings.TrimSpace(s))
		check(err)
		program = append(program, int64(i))
	}

	tileColors := runProgram(program)

	blockCount := 0
	for _, tileType := range tileColors {
		if tileType == 2 {
			blockCount++
		}
	}

	fmt.Printf("Part 1: %d\n", blockCount) // 309

}
