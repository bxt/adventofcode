require_relative "../day11/a_star"
require_relative "./field"

CLEAR_SCEEEN = "\e[H\e[2J"

class Grid < Field
  attr_reader :move_count
  attr_reader :goal_at

  def self.parse(input)
    node_data = input.lines.map do |line|
      if m = line.match(/^\/dev\/grid\/node-x(?<x>\d+)-y(?<y>\d+) +(?<size>\d+)T +(?<used>\d+)T +(?<available>\d+)T +\d+%$/)
        m.named_captures.transform_values(&:to_i)
      end
    end.compact

    array = node_data.each_with_object([]) do |node_datum, grid|
      x, y = node_datum.values_at("x", "y")
      grid[y] ||= []
      grid[y][x] = [:size, :used, :available].each_with_object({}) do |sym, hash|
        hash[sym] = node_datum[sym.to_s]
      end
    end

    new(array)
  end

  def initialize(array)
    super(array)
    @move_count = 0
    @goal_at = [width - 1, 0]
  end

  def invalid_move?(from, to)
    if from == to
      "Source and target are both #{from}"
    elsif !in_bounds?(from)
        "Source #{from} is out of bounds"
    elsif !in_bounds?(to)
        "Target #{from} is out of bounds"
    elsif !four_neighbours(from).include?(to)
        "#{from} is not adjecent to #{to}"
    elsif get(from)[:used] == 0
      "#{from} already has no data"
    elsif get(to)[:available] < get(from)[:used]
      "Not enough space on #{to} to take data from #{from}"
    end
  end

  def move(from, to)
    if error = invalid_move?(from, to)
      raise error
    end
    shuffle_size = get(from)[:used]
    get(from)[:used] -= shuffle_size
    get(from)[:available] += shuffle_size
    get(to)[:used] += shuffle_size
    get(to)[:available] -= shuffle_size
    if @goal_at == from
      @goal_at = to
    end
    @move_count += 1
  end

  def count_viable_pairs
    each_node.map do |a, a_coords|
      each_node.count do |b, b_coords|
        a_coords != b_coords && a[:used] > 0 && a[:used] <= b[:available]
      end
    end.sum
  end

  def smallest_size
    @smallest_size ||= each_node.map do |node|
      node[:size]
    end.min
  end

  def to_s
    array.each_with_index.map do |row, y|
      row.each_with_index.map do |node, x|
        if x == 0 && y == 0
          "o"
        elsif @goal_at == [x, y]
          "G"
        elsif node[:used] == 0
          "_"
        elsif node[:used] > smallest_size
          "#"
        else
          "."
        end
      end.join(" ")
    end.join("\n")
  end
end

class ComeToDaddy
  attr_accessor :grid

  def initialize(grid)
    @grid = grid
  end

  def steps
    path.each_cons(2).map(&:reverse).to_a
  end

  def path
    move_empty_space_in_front_of_goal + move_goal_in_front_of_acessible_node + [[1, 0]]
  end

  def move_empty_space_in_front_of_goal
    a_star = AStar.new(self, empty_node_coords) do |coords|
      grid.manhattan_distance(coords, space_in_front_of_goal)
    end
    path = a_star.path(space_in_front_of_goal)
  end

  def neighbours_with_distance(coords)
    grid.four_neighbours(coords).find_all(&grid.method(:in_bounds?)).find_all do |coords|
      grid.get(coords)[:used] <= grid.smallest_size
    end.map do |coords|
      [1, coords]
    end
  end

  def empty_node_coords
    grid.each_node.find do |node, coords|
      node[:used] == 0
    end[1]
  end

  def space_in_front_of_goal
    @space_in_front_of_goal ||= [grid.goal_at[0] - 1, grid.goal_at[1]]
  end

  def move_goal_in_front_of_acessible_node
    grid.goal_at[0].downto(2).flat_map do |i|
      [
        [i, 0],
        [i, 1],
        [i-1, 1],
        [i-2, 1],
        [i-2, 0],
      ]
    end.to_a
  end
end

grid = Grid.parse(IO.read("input.txt"))
# puts grid.to_s

PROMPT = "white-rabbit@storagecluster01:~> "

solver = ComeToDaddy.new(grid)

solver.steps.each do |step|
  puts CLEAR_SCEEEN
  puts "#{PROMPT}dd if=/dev/grid/node-x#{step[0][0]}-y#{step[0][1]} of=/dev/grid/node-x#{step[0][0]}-y#{step[0][1]} bs=1T"
  grid.move(*step)
  puts "#{PROMPT}sh .gift-from-santa/print-grid-status.sh"
  puts grid.to_s
  puts "#{PROMPT}sleep 0.1 && clear"
  sleep 0.1
end

raise "Doh!" unless grid.goal_at == [0, 0]

puts "Part One: #{grid.count_viable_pairs}" # 901
puts "Part Two: #{grid.move_count}"
