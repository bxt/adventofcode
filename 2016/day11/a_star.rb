require_relative "heap"
require_relative "field_support"

class AStar
  def initialize(graph, from, max_distance = nil, &min_distance)
    @graph = graph
    @from = from
    @max_distance = max_distance
    @min_distance = min_distance

    @predecessors = {}
    @known_distances = {}

    @best_distances = Hash.new { Float::INFINITY }
    @best_distances[@from] = 0

    @queue = Heap.comparing_by { |x| @best_distances[x] + @min_distance.call(x) }
    @queue.push(@from)
  end

  def reachable
    calculate_distances
    @known_distances.keys
  end

  def distance(to)
    @known_distances[to] || calculate_distances do |node, distance|
      if node == to
        @queue.push(node)
        return distance
      end
    end
  end

  def calculate_distances
    while @queue.any?
      current = @queue.pop
      current_distance = @best_distances[current]
      @known_distances[current] = current_distance
      yield current, current_distance if block_given?
      handle_children(current)
    end
  end

  def path(to)
    if distance(to)
      path = []
      while to
        path.unshift(to)
        to = @predecessors[to]
      end
      path
    end
  end

  private

  def handle_children(current)
    @graph.neighbours_with_distance(current).each do |distance, neighbour|
      unless @known_distances.key?(neighbour)
        possible_distance = @known_distances[current] + distance
        best_distance = @best_distances[neighbour]
        if possible_distance < best_distance && (@max_distance.nil? || possible_distance <= @max_distance)
          @predecessors[neighbour] = current
          @queue.updates(neighbour) do
            @best_distances[neighbour] = possible_distance
            neighbour
          end
        end
      end
    end
  end
end

if defined? RSpec
  describe AStar do

    field_class = Class.new do
      include FieldSupport
      attr_reader :field

      def initialize(field)
        @field = field
      end

      def width
        field.first.size
      end

      def height
        field.size
      end

      def distance(from, to)
        euclidian_distance(from, to)
      end

      def is_wall?(coords)
        x, y = coords
        field[y][x] == "x"
      end

      def neighbours(of)
        eight_neighbours(of).find_all(&method(:in_bounds?)).reject(&method(:is_wall?))
      end

      def self.run_a_star(use_heuristics, field_array)
        field = new(field_array)
        to = [field.width - 1, field.height - 1]
        from = [0, 0]

        a_star = AStar.new(field, from) { |x| use_heuristics ? field.distance(x, to) : 0 }

        [a_star.distance(to), a_star.path(to), a_star.reachable]
      end
    end

    context "for a simple example" do
      let(:field_array) { [
        "      ".chars,
        "xxxx  ".chars,
        "    xx".chars,
        "   x  ".chars,
      ] }

      it 'works without heurisitcs' do
        distance, path, reachable = field_class.run_a_star(false, field_array)
        expect(path).to eq([
          [0, 0],
          [1, 0],
          [2, 0],
          [3, 0],
          [4, 1],
          [3, 2],
          [4, 3],
          [5, 3],
        ])
        expect(distance).to be_within(0.000001).of(4 + 3 * Math.sqrt(2))
        expect(reachable.size).to eq(6 * 4 - 7)
      end

      it 'works with heurisitcs' do
        distance, path, reachable = field_class.run_a_star(true, field_array)
        expect(path).to eq([
          [0, 0],
          [1, 0],
          [2, 0],
          [3, 0],
          [4, 1],
          [3, 2],
          [4, 3],
          [5, 3],
        ])
        expect(distance).to be_within(0.000001).of(4 + 3 * Math.sqrt(2))
        expect(reachable.size).to eq(6 * 4 - 7)
      end
    end

    context "for another example" do
      let(:field_array) { [
        "      ".chars,
        "    x ".chars,
        "  xxx ".chars,
        "      ".chars,
      ] }

      it 'works without heurisitcs' do
        distance, path, reachable = field_class.run_a_star(false, field_array)
        expect(path).to eq([
          [0, 0],
          [0, 1],
          [1, 2],
          [2, 3],
          [3, 3],
          [4, 3],
          [5, 3],
        ])
        expect(distance).to be_within(0.000001).of(4 + 2 * Math.sqrt(2))
        expect(reachable.size).to eq(6 * 4 - 4)
      end

      it 'works with heurisitcs' do
        distance, path, reachable = field_class.run_a_star(true, field_array)
        expect(path).to eq([
          [0, 0],
          [1, 1],
          [1, 2],
          [2, 3],
          [3, 3],
          [4, 3],
          [5, 3],
        ])
        expect(distance).to be_within(0.000001).of(4 + 2 * Math.sqrt(2))
        expect(reachable.size).to eq(6 * 4 - 4)
      end
    end

    context "for a walled example" do
      let(:field_array) { [
        "    x ".chars,
        " x  x ".chars,
        " x  x ".chars,
        "    x ".chars,
      ] }

      it 'reports no path' do
        distance, path, reachable = field_class.run_a_star(true, field_array)
        expect(distance).to be_nil
        expect(path).to be_nil
        expect(reachable.size).to eq(4 * 4 - 2)
      end
    end
  end
end
