
require "./heap"

class AStar
  def initialize(graph, from, &min_distance)
    @graph = graph
    @from = from
    @min_distance = min_distance
    @predecessors = {}
    @known_distances = {}
    @best_distances = Hash.new { Float::INFINITY }
    @best_distances[@from] = 0
    @queue = Heap.comparing_by { |x| @best_distances[x] + @min_distance.call(x) }
    @queue.push(@from)
  end

  def distance(to)
    while @queue.any?
      current = @queue.pop
      @known_distances[current] = @best_distances[current]
      if current == to
        return @known_distances[to]
      else
        handle_children(current)
      end
    end
  end

  def path(to)
    distance(to)
    path = []
    while to
      path.unshift(to)
      to = @predecessors[to]
    end
    path
  end

  private

  def handle_children(current)
    @graph.neighbours_with_distance(current).each do |distance, neighbour|
      unless @known_distances.key?(neighbour)
        possible_distance = @known_distances[current] + distance
        best_distance = @best_distances[neighbour]
        if possible_distance < best_distance
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
    class Field
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
        x1, y1 = from
        x2, y2 = to
        Math.sqrt((x1 - x2) ** 2 + (y1 - y2) ** 2)
      end

      def neighbours(of)
        x, y = of
        [1,-1,0].repeated_permutation(2).find_all do |x|
          x != [0,0]
        end.map do |xDiff, yDiff|
          [x + xDiff, y + yDiff]
        end.find_all do |x, y|
          y >= 0 && y < height && x >= 0 && x < width && field[y][x] != "x"
        end
      end

      def neighbours_with_distance(of)
        neighbours(of).map do |node|
          [distance(of, node), node]
        end
      end

      def self.run_a_star(use_heuristics, field_array)
        field = Field.new(field_array)
        to = [field.width - 1, field.height - 1]
        from = [0, 0]

        a_star = AStar.new(field, from) { |x| use_heuristics ? field.distance(x, to) : 0 }

        [a_star.distance(to), a_star.path(to)]
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
        distance, path = Field.run_a_star(false, field_array)
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
      end

      it 'works with heurisitcs' do
        distance, path = Field.run_a_star(true, field_array)
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
        distance, path = Field.run_a_star(false, field_array)
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
      end

      it 'works with heurisitcs' do
        distance, path = Field.run_a_star(true, field_array)
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
      end
    end
  end
end
