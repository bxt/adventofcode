require 'set'

class TaxiCab
  DIRECTIONS = [[1, 0], [0, 1], [-1, 0], [0, -1]]
  attr_reader :position

  def initialize()
    @position = [0, 0]
    @direction = 0
  end

  def step()
    x, y = @position
    xOffset, yOffset = heading
    @position = [xOffset + x, yOffset + y]
  end

  def turn(turn_direction)
    turn_by({r: 1, l: -1}[turn_direction])
  end

  def distance
    @position.map(&:abs).inject(:+)
  end

  private

  def turn_by(offset)
    @direction = (@direction + offset) % DIRECTIONS.size
  end

  def heading
    DIRECTIONS[@direction]
  end
end

def load_instruction_file(file_name)
  file_contents = File.read(file_name)
  file_contents.split(", ").map do |instruction_string|
    {
      turn: instruction_string[0].downcase.to_sym,
      distance: instruction_string[1..-1].to_i,
    }
  end
end

peugeot406 = TaxiCab.new

instructions = load_instruction_file("input.txt")

visited_positions = Set.new
part_two_answer = nil

instructions.each do |instruction|
  peugeot406.turn(instruction[:turn])
  instruction[:distance].times do
    peugeot406.step
    unless visited_positions.add?(peugeot406.position)
      part_two_answer ||= peugeot406.distance
    end
  end
end

puts "Part One: #{peugeot406.distance}"
puts "Part Two: #{part_two_answer}"
