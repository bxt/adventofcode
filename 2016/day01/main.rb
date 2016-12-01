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

instructions.each do |instruction|
  peugeot406.turn(instruction[:turn])
  instruction[:distance].times { peugeot406.step }
end

puts peugeot406.distance
