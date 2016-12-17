require 'digest/md5'

require_relative "../day11/a_star"

DIRECTIONS = {
  "U" => [0,  -1],
  "D" => [0,  1],
  "L" => [-1, 0],
  "R" => [1,  0],
}

class Field
  include FieldSupport

  attr_reader :width, :height, :input

  def initialize(width, height, input)
    @width = width
    @height = height
    @input = input
  end

  def distance(from, to)
    manhattan_distance(from, to)
  end

  def neighbours_with_distance(of)
    coords, path = of
    return [] if coords == target
    doors = open_doors_at(path)
    DIRECTIONS.map do |direction, offset|
      next unless doors.include?(direction)
      new_coords = add_coordinates(coords, offset)
      next unless in_bounds?(new_coords)
      [1, [new_coords, path + direction]]
    end.compact
  end

  def open_doors_at(path)
    checksum = Digest::MD5.hexdigest(input + path)
    DIRECTIONS.keys.zip(checksum.chars).find_all do |_, c|
      "bcdef"[c]
    end.map(&:first)
  end

  def target
    [width - 1, height - 1]
  end

  def from
    [[0, 0], ""]
  end

  def a_star
    @a_star ||= AStar.new(self, from) do |coords, path|
      manhattan_distance(target, coords)
    end
  end

  def shortest_path_to_target
    closest = a_star.find_closest do |coords, path|
      coords == target
    end
    closest[1]
  end

  def longest_path_to_target
    reachable = a_star.reachable
    reachable.find_all do |coords, path|
      coords == target
    end.max_by do |coords, path|
      path.length
    end[1]
  end
end

input = "mmsxrhfx"

field = Field.new(4, 4, input)

puts "Part One: #{field.shortest_path_to_target}" # RLDUDRDDRR
# runs for ~35s
puts "Part Two: #{field.longest_path_to_target.length}" # 590
