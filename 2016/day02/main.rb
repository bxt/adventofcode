
class Keypad
  attr_reader :position
  attr_reader :keypad

  def initialize(keypad, position)
    @keypad = keypad
    @position = position
  end

  def key_at(position)
    x, y = position
    if y >= 0 && y < keypad.size && x >= 0 && x < keypad[y].size
      keypad[y][x]
    end
  end

  def current_key
    key_at(@position)
  end

  def move(offset)
    new_position = @position.zip(offset).map { |a| a.inject(:+) }
    if key_at(new_position)
      @position = new_position
    end
  end
end

PARTS = {
  "One" => {
    keypad: [
      [1, 2, 3],
      [4, 5, 6],
      [7 ,8, 9],
    ],
    position: [1,1],
  },
  "Two" => {
    keypad: [
      [nil, nil, 1,   nil, nil],
      [nil, 2,   3,   4,   nil],
      [5,   6,   7,   8,   9],
      [nil, "A", "B", "C", nil],
      [nil, nil, "D", nil, nil],
    ],
    position: [0,2],
  },
}

DIRECTIONS = {
  "U" => [0,  -1],
  "D" => [0,  1],
  "L" => [-1, 0],
  "R" => [1,  0],
}

PARTS.each do |part, options|
  keypad = Keypad.new(*options.values_at(:keypad, :position))

  keys = IO.readlines("input.txt").map do |line|
    line.chars.each do |char|
      unless DIRECTIONS[char].nil?
        keypad.move(DIRECTIONS[char])
      end
    end
    keypad.current_key
  end

  puts "Part #{part}: #{keys.map(&:to_s).join}"
end
