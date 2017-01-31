
module OpCodes
  def cpy(operands)
    from, to = operands
    regs[to] = eval_operand(from) if regs.key?(to)
    inc_instruction_pointer
  end

  def inc(operands)
    reg, = operands
    regs[reg] += 1 if regs.key?(reg)
    inc_instruction_pointer
  end

  def dec(operands)
    reg, = operands
    regs[reg] -= 1 if regs.key?(reg)
    inc_instruction_pointer
  end

  def jnz(operands)
    check, offset = operands
    if eval_operand(check) != 0
      inc_instruction_pointer(eval_operand(offset))
    else
      inc_instruction_pointer
    end
  end
end

class Machine
  attr_accessor :regs, :code

  def initialize(code)
    @regs = "abcd".chars.each_with_object({}) { |x, h| h[x] = 0 }
    @instruction_pointer = 0
    @instruction_counter = 0
    @output = []
    parse(code)
  end

  def run(max_instructions = Float::INFINITY)
    max_instruction_counter = @instruction_counter + max_instructions
    while @instruction_pointer < code.size && @instruction_counter < max_instruction_counter
      opcode, operands = code[@instruction_pointer]
      send(opcode, operands)
      @instruction_counter += 1
    end
  end

  def done?
    @instruction_pointer >= code.size
  end

  def result
    regs["a"]
  end

  private

  include OpCodes

  def inc_instruction_pointer(by = 1)
    @instruction_pointer = @instruction_pointer + by
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
