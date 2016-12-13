require_relative './a_star'

class Field
  def initialize(from, target, input)
    @from = from
    @target = target
    @input = input
  end

  def distance(from, to)
    x1, y1 = from
    x2, y2 = to
    Math.sqrt((x1 - x2) ** 2 + (y1 - y2) ** 2)
  end

  def neighbours(of)
    x, y = of
    #    [1,-1,0].repeated_permutation(2).find_all do |x|
    [[1,0],[0,1],[-1,0],[0,-1]].map do |xDiff, yDiff|
      [x + xDiff, y + yDiff]
    end.find_all do |x, y|
      y >= 0 && x >= 0 && !is_wall?(x, y)
    end
  end

  def is_wall?(x, y)
    forumula_result = x*x + 3*x + 2*x*y + y + y*y
    forumula_result += @input
    forumula_result.to_s(2).chars.count { |c| c == '1' }.odd?
  end

  def neighbours_with_distance(of)
    neighbours(of).map do |node|
      [distance(of, node), node]
    end
  end

  def run_a_star
    from = @from
    to = @target

    a_star = AStar.new(self, from) { |x| distance(x, to) }

    @distance = a_star.distance(to)
    @path = a_star.path(to)

    [@distance, @path]
  end

  def reachable
    from = @from
    to = @target

    a_star = AStar.new(self, from, 50) { |x| distance(x, to) }

    a_star.reachable
  end

  def to_s(to = [10, 10])
    x_max, y_max = to
    y_max.times.map do |y|
      x_max.times.map do |x|
        is_wall?(x, y) ? '#' : ((@path && @path.include?([x, y])) ? 'O' : ' ')
      end.join
    end.join("\n")
  end
end

from = [1,1]

target = [31,39]
input = 1352

#target = [7,4]
#input = 10

field = Field.new(from, target, input)

puts field.to_s([10, 7])

x = field.reachable

puts x
