require 'set'
require_relative "../../2016/day04/frequencies"

file_contents = File.read('input.txt')
input = file_contents.split("\n").map{|x| x.split(", ").map(&:to_i)}

minX = input.map{|(x, _)| x}.min
maxX = input.map{|(x, _)| x}.max
minY = input.map{|(_, y)| y}.min
maxY = input.map{|(_, y)| y}.max

class Grid < Struct.new(:minX, :maxX, :minY, :maxY, :data)
  def each
    (minX..maxX).each do |x|
      (minY..maxY).each do |y|
        point = [x, y]
        yield get(point), point
      end
    end
  end

  def get(p)
    contains?(p) ? data[index(p)] : nil
  end

  def set(p, v)
    raise "out of bounds" unless contains?(p)
    data[index(p)] = v
  end

  def contains?(point)
    x, y = point
    x >= minX && x <= maxX && y >= minY && y <= maxY
  end

  def is_on_border?(point)
    x, y = point
    x == minX || x == maxX || y == minY || y == maxY
  end

  private

  def index(point)
    x, y = point
    (x - minX) * (maxX - minX) + (y - minY)
  end
end

def manhattan_distance(p1, p2)
  x1, y1 = p1
  x2, y2 = p2
  (x2-x1).abs + (y2-y1).abs
end

grid = Grid.new(minX, maxX, minY, maxY, [nil])

grid.each do |element, point|
  x, y = point

  with_distances = input.map do |input_point|
    [manhattan_distance(input_point, point), input_point]
  end

  minimum_distance, one_closest_point = with_distances.min

  is_tie = with_distances.map(&:first).select do |d|
    d == minimum_distance
  end.length > 1

  grid.set(point, is_tie ? :tie : one_closest_point)
end

grid.each do |element, point|
  x, y = point

  with_distances = input.map do |input_point|
    [manhattan_distance(input_point, point), input_point]
  end

  minimum_distance, one_closest_point = with_distances.min

  is_tie = with_distances.map(&:first).select do |d|
    d == minimum_distance
  end.length > 1

  grid.set(point, is_tie ? :tie : one_closest_point)
end

infinites = Set.new

grid.each do |element, point|
  infinites.add(element) if grid.is_on_border?(point)
end

fs = frequencies(grid.data)

result = fs.map(&:reverse).sort.select do |(_, point)|
  !infinites.include?(point)
end

puts "Part One: #{result.last.first}"

number_of_points_with_safe_distance_total = 0

grid.each do |element, point|
  x, y = point

  distances_total = input.map do |input_point|
    manhattan_distance(input_point, point)
  end.sum

  if distances_total < 10000
    number_of_points_with_safe_distance_total += 1
  end
end

puts "Part Two: #{number_of_points_with_safe_distance_total}"
