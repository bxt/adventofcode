require "set"

class TaxiCab
  DIRECTIONS = [{1, 0}, {0, 1}, {-1, 0}, {0, -1}]

  def initialize
    @position = {0, 0}
    @direction = 0
  end

  def position
    @position
  end

  def step
    @position = Tuple(Int32, Int32).from(@position.zip(heading).map(&.sum))
  end

  def turn(turn_direction)
    turn_by({r: 1, l: -1}[turn_direction])
  end

  def distance
    @position.map(&.abs).sum
  end

  private def turn_by(offset)
    @direction = (@direction + offset) % DIRECTIONS.size
  end

  private def heading
    DIRECTIONS[@direction]
  end
end

def load_instruction_file(file_name)
  file_contents = File.read(file_name)
  file_contents.split(", ").map do |instruction_string|
    {
      turn: instruction_string[0] == 'L' ? :l : :r,
      distance: instruction_string[1..-1].to_i,
    }
  end
end

peugeot406 = TaxiCab.new

instructions = load_instruction_file("input.txt")

visited_positions = Set({Int32, Int32}).new
part_two_answer = nil

instructions.each do |instruction|
  peugeot406.turn(instruction[:turn])
  instruction[:distance].times do
    peugeot406.step
    if visited_positions.includes?(peugeot406.position)
      part_two_answer ||= peugeot406.distance
    end
    visited_positions.add(peugeot406.position)
  end
end

puts "Part One: #{peugeot406.distance}"
puts "Part Two: #{part_two_answer}"
