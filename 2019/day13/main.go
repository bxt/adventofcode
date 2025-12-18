package main

import (
	"bufio"
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

type io interface {
	emitOutput(value int64)
	dequeueInput() int64
}

type painter struct {
	task            task
	score           int64
	currentPosition coord
	tileColors      map[coord]int64
}

func makePainter() painter {
	return painter{
		task:            x,
		currentPosition: coord{x: 0, y: 0},
		tileColors:      make(map[coord]int64),
	}
}

func (p *painter) emitOutput(value int64) {
	switch p.task {
	case paint:
		if p.currentPosition.x == -1 && p.currentPosition.y == 0 {
			p.score = value
		} else {
			p.tileColors[p.currentPosition] = value
		}
	case x:
		p.currentPosition = coord{x: int(value), y: p.currentPosition.y}
	case y:
		p.currentPosition = coord{x: p.currentPosition.x, y: int(value)}
	default:
		panic(fmt.Sprintf("Unknown task: %d", p.task))
	}
	p.task = (p.task + 1) % taskCount
}

type sittingDuckPainter struct {
	painter
}

func (p *sittingDuckPainter) dequeueInput() int64 {
	return 0
}

type interactivePainter struct {
	painter
	part1Result int
}

func (p *interactivePainter) dequeueInput() int64 {
	fmt.Print("\033[H\033[2J")

	fmt.Printf("Part 1: %d\n", p.part1Result)

	minX, maxX := 0, 0
	minY, maxY := 0, 0
	for c := range p.tileColors {
		if c.x < minX {
			minX = c.x
		}
		if c.x > maxX {
			maxX = c.x
		}
		if c.y < minY {
			minY = c.y
		}
		if c.y > maxY {
			maxY = c.y
		}
	}

	for y := minY; y <= maxY; y++ {
		for x := minX; x <= maxX; x++ {
			c := coord{x: x, y: y}
			color, exists := p.tileColors[c]
			if !exists {
				fmt.Print(" ")
				continue
			}
			switch color {
			case 0:
				fmt.Print(" ")
			case 1:
				fmt.Print("#")
			case 2:
				fmt.Print("\033[33m#\033[0m")
			case 3:
				fmt.Print("\033[31m=\033[0m")
			case 4:
				fmt.Print("\033[32mO\033[0m")
			default:
				panic(fmt.Sprintf("Unknown color: %d", color))
			}
		}
		fmt.Println()
	}

	fmt.Printf("\n\nPart 2: %d\n", p.score)

	reader := bufio.NewReader(os.Stdin)
	fmt.Print("Enter text: ")
	text, _ := reader.ReadString('\n')
	fmt.Println(text)

	if strings.Contains(text, "a") {
		return -1
	}
	if strings.Contains(text, "d") {
		return 1
	}

	return 0
}

type machine struct {
	state          []int64
	programCounter int64
	relativeBase   int64
	io             io
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

func makeMachine(program []int64, io io) *machine {
	return &machine{
		state: append(program[:0:0], program...),
		io:    io,
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
			m.writeOperand(1, m.io.dequeueInput())
			m.advanceProgramCounter(2)
		case out:
			m.io.emitOutput(m.resolveOperand(1))
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

	ioPart1 := &sittingDuckPainter{
		painter: makePainter(),
	}
	mPart1 := makeMachine(program, ioPart1)
	mPart1.run()

	blockCount := 0
	for _, tileType := range ioPart1.tileColors {
		if tileType == 2 {
			blockCount++
		}
	}

	fmt.Printf("Part 1: %d\n", blockCount) // 309

	ioPart2 := &interactivePainter{
		painter:     makePainter(),
		part1Result: blockCount,
	}
	mPart2 := makeMachine(program, ioPart2)
	mPart2.state[0] = 2
	mPart2.run()

	fmt.Printf("Part 2: %d\n", ioPart2.score) // 12341

}
