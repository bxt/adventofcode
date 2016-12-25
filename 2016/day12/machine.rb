
module OpCodes
  def cpy(operands)
    from, to = operands
    regs[to] = eval_operand(from) if regs.key?(to)
    inc_ic
  end

  def inc(operands)
    reg, = operands
    regs[reg] += 1 if regs.key?(reg)
    inc_ic
  end

  def dec(operands)
    reg, = operands
    regs[reg] -= 1 if regs.key?(reg)
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
