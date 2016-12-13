module FieldSupport
  def euclidian_distance(from, to)
    x1, y1 = from
    x2, y2 = to
    Math.sqrt((x1 - x2) ** 2 + (y1 - y2) ** 2)
  end

  def manhattan_distance(from, to)
    x1, y1 = from
    x2, y2 = to
    (x1 - x2).abs + (y1 - y2).abs
  end

  def eight_neighbours(of)
    x, y = of
    [1,-1,0].repeated_permutation(2).find_all do |x|
      x != [0,0]
    end.map do |x_diff, y_diff|
      [x + x_diff, y + y_diff]
    end
  end

  def four_neighbours(of)
    x, y = of
    [[1,0],[0,1],[-1,0],[0,-1]].map do |x_diff, y_diff|
      [x + x_diff, y + y_diff]
    end
  end

  def neighbours_with_distance(of)
    neighbours(of).map do |node|
      [distance(of, node), node]
    end
  end
end

if defined? RSpec
  describe FieldSupport do
    subject do
      class DUT
        include FieldSupport
      end.new
    end

    describe "#euclidian_distance(from, to)" do
      examples = {
        [[1, 1], [1, 3]] => 2,
        [[1, 1], [2, 2]] => Math.sqrt(2),
      }
      examples.each do |points, distance|
        it "returns #{distance} for #{points.inspect}" do
          expect(subject.euclidian_distance(*points)).to be_within(0.000001).of(distance)
        end
      end
    end

    describe "#manhattan_distance(from, to)" do
      examples = {
        [[1, 1], [1, 3]] => 2,
        [[1, 1], [2, 2]] => 2,
      }
      examples.each do |points, distance|
        it "returns #{distance} for #{points.inspect}" do
          expect(subject.manhattan_distance(*points)).to eq(distance)
        end
      end
    end

    describe "#eight_neighbours(of)" do
      it "returns the adjacent and diagonally adjacent pixels" do
        neighbours = [[4, 3], [3, 4], [2, 3], [3, 2], [4, 2], [2, 4], [4, 4], [2, 2]]
        expect(subject.eight_neighbours([3, 3])).to match_array(neighbours)
      end
    end

    describe "#four_neighbours(of)" do
      it "returns the adjacent pixels" do
        neighbours = [[4, 3], [3, 4], [2, 3], [3, 2]]
        expect(subject.four_neighbours([3, 3])).to match_array(neighbours)
      end
    end

    describe "#neighbours_with_distance(of)" do
      it "returns the neighbours from #neighbours(of) with their distance" do
        expect(subject).to receive(:neighbours).with(:of).and_return([:to])
        expect(subject).to receive(:distance).with(:of, :to).and_return(:distance)
        expect(subject.neighbours_with_distance(:of)).to eq([[:distance, :to]])
      end
    end
  end
end
