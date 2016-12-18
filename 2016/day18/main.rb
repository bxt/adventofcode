
class TiledFloor
  SAFE_STR = "."
  TRAP_STR = "^"
  attr_reader :tiles

  def initialize(first_row)
    @tiles = [first_row]
  end

  def calculate_rows(upto)
    while tiles.size < upto
      tiles.push(next_row)
    end
  end

  def next_row
    ([true] + tiles.last + [true]).each_cons(3).map do |left, center, right|
      left == right
    end
  end

  def count_safe_tiles
    tiles.map do |row|
      row.count(&:itself)
    end.inject(&:+)
  end

  def to_s
    tiles.map do |row|
      row.map do |tile|
        if tile
          SAFE_STR
        else
          TRAP_STR
        end
      end.join
    end.join("\n")
  end

  def self.parse(first_row_str)
    parsed = first_row_str.chars.map do |c|
      c == SAFE_STR
    end
    new(parsed)
  end
end

input = IO.read("input.txt").strip
# input = ".^^.^.^^^^"

tiled_floor = TiledFloor.parse(input)
tiled_floor.calculate_rows(40)
# puts tiled_floor.to_s
puts "Part One: #{tiled_floor.count_safe_tiles}"

tiled_floor.calculate_rows(400000)
puts "Part Two: #{tiled_floor.count_safe_tiles}"
