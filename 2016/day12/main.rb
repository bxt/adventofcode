require_relative "./machine"

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
