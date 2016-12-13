require_relative '../day11/a_star'

class Field
  include FieldSupport

  def initialize(from, input)
    @from = from
    @input = input
  end

  def distance(from, to)
    manhattan_distance(from, to)
  end

  def neighbours(of)
    four_neighbours(of).find_all do |x, y|
      y >= 0 && x >= 0 && !is_wall?(x, y)
    end
  end

  def is_wall?(x, y)
    forumula_result = x*x + 3*x + 2*x*y + y + y*y
    forumula_result += @input
    forumula_result.to_s(2).chars.count { |c| c == '1' }.odd?
  end

  def reachable
    a_star.reachable
  end

  def to_s
    x_max = reachable.map(&:first).max + 2
    y_max = reachable.map { |x| x[1] }.max + 2
    path = a_star.path(@target) if defined?(@target)
    y_max.times.map do |y|
      x_max.times.map do |x|
        to_s_point(x, y, path)
      end.join
    end.join("\n")
  end

  def to_s_point(x, y, path)
    case true
    when is_wall?(x, y)
      '#'
    when path && path.include?([x, y])
      'O'
    when reachable.include?([x, y])
      '.'
    else
      ' '
    end
  end
end

class FieldPartOne < Field
  def initialize(from, input, target)
    super(from, input)
    @target = target
  end

  def a_star
    @a_star ||= AStar.new(self, @from) { |x| distance(x, @target) }
  end

  def distance_to_target
    a_star.distance(@target)
  end
end

class FieldPartTwo < Field
  def initialize(from, input, range)
    super(from, input)
    @range = range
  end

  def a_star
    @a_star ||= AStar.new(self, @from, @range) { |x| 0 }
  end
end

from = [1,1]
input = 1352
target = [31,39]
range = 50

# -- Example:
# target = [7,4]
# input = 10

field_one = FieldPartOne.new(from, input, target)
puts "Part One: #{field_one.distance_to_target}"
# puts field_one.to_s


field_two = FieldPartTwo.new(from, input, range)
puts "Part Two: #{field_two.reachable.size}"
# puts field_two.to_s
