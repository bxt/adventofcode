file = File.read("input.txt")

instructions = file.split(", ").map do |instruction_string|
  {
    turn: instruction_string[0].downcase.to_sym,
    distance: instruction_string[1..-1].to_i,
  }
end

directions = [[1, 0], [0, 1], [-1, 0], [0, -1]]

current_direction = 0

turn_direction_offset = {r: 1, l: -1}

current_position = [0, 0]

res = instructions.each do |instruction|
  x, y = current_position
  direction_offset = turn_direction_offset[instruction[:turn]]
  current_direction = (current_direction + direction_offset) % directions.size
  xOffset, yOffset = directions[current_direction]
  distance = instruction[:distance]
  current_position = [xOffset*distance + x, yOffset*distance + y]
end

puts current_position.map(&:abs).inject(:+)
