require_relative "../day12/machine"

module OpCodes
  def break(operands)
    raise
  end
  def tgl(operands)
    offset, = operands
    position = @ic + eval_operand(offset)
    if position >= 0 && position < code.size
      opcode, operands = code[position]
      puts "toggle: ic+#{eval_operand(offset)}=#{position} (#{code[position]})"
      code[position] = case operands.size
      when 1
        if opcode == "inc"
          ["dec", operands]
        else
          ["inc", operands]
        end
      when 2
        if opcode == "jnz"
          ["cpy", operands]
        else
          ["jnz", operands]
        end
      end
    end
    inc_ic
  end
end

def get_code
  IO.read("input.txt")
  # "cpy 2 a\ntgl a\ntgl a\ntgl a\ncpy 1 a\ndec a\ndec a"
end

def run_with(eggs)
  m = Machine.new(get_code)
  m.regs["a"] = eggs
  m.run
  m.result
end

puts "Part One: #{run_with(7)}"
puts "Part Two: #{run_with(12)}" # runs 35 min, but this is less than coding sth ^^
