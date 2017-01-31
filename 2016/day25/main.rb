require_relative "../day12/machine"

module OpCodes
  attr_reader :output

  def out(operands)
    value, = operands
    @output << eval_operand(value)
    inc_instruction_pointer
  end
end

def check(output)
  output.each_with_index.all? { |n, i| n == i % 2 }
end

MAX = 3000
STEPS = 10000

code = IO.read("input.txt")
machines = []

MAX.times do |i|
  m = Machine.new(code)
  m.regs["a"] = i
  machines << [i, m]
  machines.select! do |(i, m)|
    m.run(STEPS)
    check(m.output)
  end
  puts
  puts "Left:"
  puts (machines.map do |(i, m)|
    "#{i} (#{m.output.length})"
  end.join(", "))
end
