
module OpCodes
  def cpy(operands)
    from, to = operands
    regs[to] = eval_operand(from)
    inc_ic
  end

  def inc(operands)
    regs[operands.first] += 1
    inc_ic
  end

  def dec(operands)
    regs[operands.first] -= 1
    inc_ic
  end

  def jnz(operands)
    check, offset = operands
    if eval_operand(check) != 0
      inc_ic(eval_operand(offset))
    else
      inc_ic
    end
  end
end

class Machine
  attr_accessor :regs, :code

  def initialize(code)
    @regs = "abcd".chars.each_with_object({}) { |x, h| h[x] = 0 }
    @ic = 0
    parse(code)
  end

  def run
    while @ic < code.size
      opcode, operands = code[@ic]
      send(opcode, operands)
    end
  end

  def result
    regs["a"]
  end

  private

  include OpCodes

  def inc_ic(by = 1)
    @ic = @ic + by
  end

  def parse(code)
    @code = code.lines.map do |line|
      opcode, *operands = line.strip.split.map(&:strip)
      [opcode, operands]
    end
  end

  def eval_operand(operand)
    if (regs.key?(operand))
      regs[operand]
    else
      operand.to_i
    end
  end
end

def get_code
  IO.read("input.txt")
  # "cpy 41 a\ninc a\ninc a\ndec a\njnz a 2\ndec a"
end

def part1
  m = Machine.new(get_code)
  m.run
  m.result
end

def part2
  m = Machine.new(get_code)
  m.regs["c"] = 1
  m.run
  m.result
end

puts "Part One: #{part1}"
puts "Part Two: #{part2}"
