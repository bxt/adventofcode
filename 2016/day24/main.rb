require_relative "../day11/a_star"
require_relative "../day22/field"

class Grid < Field
  def self.parse(input)
    new(input.lines.map(&:strip).map(&:chars))
  end

  def initialize(array)
    super(array)
  end

  def distance(from, to)
    manhattan_distance(from, to)
  end

  def neighbours(of)
    four_neighbours(of).find_all(&method(:in_bounds?)).reject(&method(:is_wall?))
  end

  def is_wall?(coords)
    get(coords) == "#"
  end

  def shortest_trip_length(roundtrip)
    targets.keys.reject(&start_at.method(:==)).permutation.map do |trip|
      trip_length([start_at] + trip + (roundtrip ? [start_at] : []))
    end.min
  end

  def trip_length(trip)
    trip.each_cons(2).map do |ts|
      a, b = ts.sort
      distance_table[a][b]
    end.sum
  end

  def distance_table
    @distance_table ||= targets.keys.combination(2)
        .each_with_object(empty_distance_table) do |ts, dt|
      a, b = ts.sort
      dt[a][b] = a_stars[a].distance(targets[b])
    end
  end

  def targets
    @targets ||= each_node.find_all do |node, coords|
      /[0-9]/.match?(node)
    end.each_with_object({}) do |(node, coords), hash|
      hash[node.to_i] = coords
    end
  end

  def start_at
    0
  end

  private

  def a_stars
    targets.each_with_object({}) do |(node, coords), hash|
      hash[node] = AStar.new(self, coords) { 0 }
    end
  end

  def empty_distance_table
    Hash.new { |hash, key| hash[key] = {} }
  end
end

grid = Grid.parse(IO.read("input.txt"))

puts "Part One: #{grid.shortest_trip_length(false)}"
puts "Part Two: #{grid.shortest_trip_length(true)}"
