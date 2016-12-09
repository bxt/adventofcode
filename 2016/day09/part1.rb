
class StateMachine
  attr_reader :input, :output

  def initialize(input)
    @state = self.class.starting_state
    @input = input
    @output = []
  end

  def run
    input.chars.each { |char| step(char) }
  end

  def step(char)
    @state = send(:"step_#{@state}", char)
  end

  def stay
    @state
  end

  def transition_to(state)
    state
  end

  def out(char)
    output.push(char)
  end
end

module IgnoreWhitespace
  def step(char)
    if /\s/ === char
      @state
    else
      super
    end
  end
end

module NumberReader
  def step_in_number(char)
    @number_chars ||= []
    if /\d/ === char
      @number_chars.push(char)
      stay
    else
      raise "Illegal char in number: #{char}"
    end
  end

  def finish_number
    number = @number_chars.join.to_i
    @number_chars = []
    number
  end
end

class ExpanderV2 < StateMachine
  include IgnoreWhitespace
  include NumberReader

  def self.starting_state
    :default
  end

  def initialize(input)
    super
    @repeated_sequence = []
  end

  def step_default(char)
    if char == "("
      transition_to(:in_length)
    else
      out(char)
      stay
    end
  end

  def step_in_length(char)
    if char == "x"
      @length = finish_number
      transition_to(:in_factor)
    else
      step_in_number(char)
    end
  end

  def step_in_factor(char)
    if char == ")"
      @factor = finish_number
      transition_to(:in_repeated_sequence)
    else
      step_in_number(char)
    end
  end

  def step_in_repeated_sequence(char)
    if @length > 0
      @length -= 1
      @repeated_sequence.push(char)
      stay
    end
    if @length <= 0
      out(@repeated_sequence * @factor)
      @repeated_sequence = []
      transition_to(:default)
    else
      stay
    end
  end

  def to_s
    run
    output.join
  end

  def size
    to_s.size
  end
end

def part1
  ExpanderV2.new(IO.read("input.txt")).size
end

if __FILE__ == $PROGRAM_NAME
  puts part1
end

if defined? RSpec
  describe ExpanderV2 do
    describe :examples do
      it { expect(ExpanderV2.new("ADVENT").to_s).to eq("ADVENT") }
      it { expect(ExpanderV2.new("A(1x5)BC").to_s).to eq("ABBBBBC") }
      it { expect(ExpanderV2.new("(3x3)XYZ").to_s).to eq("XYZXYZXYZ") }
      it { expect(ExpanderV2.new("A(2x2)BCD(2x2)EFG").to_s).to eq("ABCBCDEFEFG") }
      it { expect(ExpanderV2.new("(6x1)(1x3)A").to_s).to eq("(1x3)A") }
      it { expect(ExpanderV2.new("X(8x2)(3x3)ABCY").to_s).to eq("X(3x3)ABC(3x3)ABCY") }
    end

    it 'ignores whitespace' do
      expect(ExpanderV2.new("ADV\nENT").to_s).to eq("ADVENT")
    end
  end

  describe :part1 do
    subject { part1 }
    it { is_expected.to eq(99145) }
  end
end
