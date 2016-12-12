
class Machine
  attr_accessor :regs, :code

  def initialize(code)
    @regs = {a: 0, b: 0, c: 1, d: 0}
    @ic = 0
    parse(code)
  end

  def parse(code)
    @code = code.lines.map do |line|
      opcode, *operands = line.strip.split.map(&:strip)
      [opcode, operands]
    end
  end

  def eval_operand(operand)
    if (regs.key?(operand.to_sym))
      regs[operand.to_sym]
    else
      operand.to_i
    end
  end

  def cpy(operands)
    from, to = operands
    regs[to.to_sym] = eval_operand(from)
    inc_ic
  end

  def inc(operands)
    regs[operands.first.to_sym] += 1
    inc_ic
  end

  def dec(operands)
    regs[operands.first.to_sym] -= 1
    inc_ic
  end

  def inc_ic
    @ic = @ic + 1
  end

  def jnz(operands)
    # puts "#{@ic}=jnz #{operands.inspect} (#{operands.map(&method(:eval_operand)).inspect})"
    check, offset = operands
    if eval_operand(check) != 0
      last_ic = @ic
      evaled_offset = eval_operand(offset)
      new_ic = last_ic + evaled_offset
      # puts "-- #{last_ic}>#{new_ic} -- #{eval_operand(check)} -- #{evaled_offset} --"
      @ic = new_ic
    else
      inc_ic
    end
  end

  def run
    while @ic < code.size
      opcode, operands = code[@ic]
      # puts regs.inspect
      # puts "@#{@ic}: #{opcode}(#{operands.inspect})"
      send(opcode, operands)
    end
    #puts @ic
  end

  def result
    regs[:a]
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
  m.regs[:c] = 1
  m.run
  m.result
end

puts "Part One: #{part1}"
puts "Part Two: #{part2}"
