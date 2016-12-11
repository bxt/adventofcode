
class Heap
  include Enumerable

  def initialize(array = [], &block)
    @comparator = block
    @comparator ||= ->(x, y) { x <=> y }
    @array = array.dup
    @array.each_index.reverse_each { |i| heapyfy(i) }
  end

	def pop
		raise 'Nothing left' if array.size <= 0
    max = array.shift
		heapyfy(0) if array.size > 0
		max
	end

  def push(element)
		array.push(element)
    i = array.size - 1
    increased(i)
	end

  def updates(element)
    index = search_index(element)
    if index
      return_value = yield
      heapyfy(index)
      increased(index)
      return_value
    else
      raise IndexError
    end
  end

  def size
    array.size
  end

  def each(&block)
    each_from(0, &block)
  end

  def to_s
		string = ""

		break_index = 0
    break_after = 1
    child_width = 1
		child_width *= 2 while (child_width * 2 <= array.size)

		array.each_with_index do |element, index|
      string += " " * (child_width * 2 + 1)
			string += element.to_s
      string += " " * ((child_width - 1) * 2)
			if index == break_index
        string += "\n"
				break_after *= 2
				break_index += break_after
				child_width /= 2;
			end
		end

		string
	end

  private

  attr_reader :array

  def swap(i1, i2)
    array[i1], array[i2] = array[i2], array[i1]
  end

  def compare(i1, i2)
    @comparator.call(array[i1], array[i2])
  end

  def parent_index(i)
    (i - 1) / 2
  end

  def left_index(i)
    i * 2 + 1
  end

  def right_index(i)
    i * 2 + 2
  end

  def each_from(index, &block)
    if index < array.size
      block.call(array[index])
      each_from(left_index(index), &block)
      each_from(right_index(index), &block)
    end
  end

	def heapyfy(i)
    largest = [i, left_index(i), right_index(i)].select do |index|
      index < array.size
    end.max(&method(:compare))

		if largest != i
			swap(i, largest)
			heapyfy(largest)
		end
	end

  def increased(i)
    while i > 0 && compare(i, parent_index(i)) > 0
			swap(i, parent_index(i))
			i = parent_index(i)
		end
  end

  def search_index(element, i = 0)
    return unless i < array.size
    return i if array[i] == element
    return if @comparator.call(element, array[i]) > 1
    search_index(element, left_index(i)) || search_index(element, right_index(i))
  end
end

if __FILE__ == $PROGRAM_NAME
	heap = Heap.new((1..9).to_a);
  puts heap.to_s
  puts heap.send(:array).inspect
  2.times { puts heap.pop }
  puts heap.to_s
  3.times { |i| heap.push(i * 8); heap.push(3) }
  puts heap.to_s
  puts heap.to_a.inspect
  puts "7@#{heap.updates(7){}}"
  puts "9@#{heap.updates(9){}}"
end

if defined? RSpec
  describe Heap do
    context "empty" do
      subject { described_class.new }

      describe "#pop" do
        it "raises an error" do
          expect { described_class.new.pop }.to raise_error("Nothing left")
        end
    	end

      describe "#push(element)" do
        it "may retrieve the passed element back with #pop" do
          subject.push(3)
          expect(subject.pop).to eq(3)
    		end

        it "changes the size by 1" do
          expect { subject.push(3) }.to change(subject, :size).from(0).to(1)
    		end
    	end

      describe "#size" do
        it "is 0" do
          expect(subject.size).to eq(0)
    		end
      end

      describe "#each(&block)" do
        it "does not call the block" do
          expect { |b| subject.each(&b) }.not_to yield_control
        end
      end

      describe "#to_s" do
    		it 'is an empty string' do
          expect(subject.to_s).to eq("")
        end
    	end
    end

    example_values = [9,4,6,1].freeze
    sorted_example_values = example_values.sort.reverse.freeze

    context "filled with values #{example_values.inspect}" do

      subject { described_class.new(example_values) }

      describe "#pop" do
    		it "repeatedly returns the maximum value" do
          sorted_example_values.each do |value|
            expect(subject.pop).to eq(value)
          end
    		end
    	end

      describe "#to_a" do
        it "returns all element in an array" do
          expect(subject.to_a).to match_array(example_values)
        end
      end

      describe "#push(element)" do
        it "may retrieve the passed element back with #pop if it is maximum" do
          subject.push(11)
          expect(subject.pop).to eq(11)
    		end

        it "changes the size by 1" do
          expect { subject.push(3) }.to change(subject, :size).by(1)
    		end

        it "may add an existing value twice" do
          expect { subject.push(example_values[1]) }.to change(subject, :size).by(1)
    		end

        it "may add number of values" do
          more_values = (100...200).to_a
          more_values.each(&subject.method(:push))
          expect(subject.to_a).to match_array(example_values + more_values)
    		end
    	end

      describe "#updates(element)" do
        it "does not change the contained elements" do
          expect { subject.updates(example_values.first) {  } }.to_not change(subject, :to_a)
        end
      end

      describe "#size" do
        it "is #{example_values.size}" do
          expect(subject.size).to eq(example_values.size)
    		end
      end

      describe "#each(&block)" do
        it "recieves all the elements" do
          yielded = []
          subject.each(&yielded.method(:push))
          expect(yielded).to match_array(example_values)
        end
      end
    end

    describe "#to_s" do
      it "returns a pyramid of values" do
        subject = described_class.new([9, 4, 6, 1])
        expect(subject.to_s).to eq([
          "         9      ",
          "     4       6  ",
          "   1",
        ].join("\n"))
      end

      it "works when the values fit exactly" do
        subject = described_class.new([9, 4, 6])
        expect(subject.to_s).to eq([
          "     9  ",
          "   4   6\n",
        ].join("\n"))
      end
    end
  end
end
