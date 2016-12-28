
def valid_triangle?(sides)
  *catheti, hypotenuse = sides.sort
  catheti.sum > hypotenuse
end

def parse_line(line)
  line.split.map(&:to_i)
end

def part_one_answer
  IO.readlines("input.txt").map(&method(:parse_line)).count(&method(:valid_triangle?))
end

def part_two_answer
  IO.readlines("input.txt").each_slice(3).map do |lines|
    lines.map(&method(:parse_line)).transpose.count(&method(:valid_triangle?))
  end.sum
end

puts "Part One: #{part_one_answer}"
puts "Part Two: #{part_two_answer}"
