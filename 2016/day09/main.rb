
class StateMachine
  attr_reader :input, :output

  def initialize(input)
    @state = self.class.starting_state
    @input = input
    @output = []
  end

  def run
    input.chars.each { |char| step(char) }
    output
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

  def emit(char)
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
      fail_number(char)
    end
  end

  def get_number
    number = @number_chars.join.to_i
    @number_chars = []
    number
  end

  def fail_number(char)
    raise "Illegal char in number: #{char}"
  end
end

class ExpanderBase < StateMachine
  include IgnoreWhitespace
  include NumberReader

  def self.starting_state
    :default
  end

  def step_default(char)
    if char == "("
      transition_to(:in_length)
    else
      finish_default(char)
    end
  end

  def step_in_length(char)
    if char == "x"
      @length = get_number
      transition_to(:in_factor)
    else
      step_in_number(char)
    end
  end

  def step_in_factor(char)
    if char == ")"
      @factor = get_number
      finish_marker(@factor, @length)
    else
      step_in_number(char)
    end
  end

end

class ExpanderV2 < ExpanderBase
  def initialize(input)
    super
    @repeated_sequence = []
  end

  def finish_default(char)
    emit(char)
    stay
  end

  def finish_marker(factor, length)
    transition_to(:in_repeated_sequence)
  end

  def step_in_repeated_sequence(char)
    if @length > 0
      @length -= 1
      @repeated_sequence.push(char)
    end
    if @length <= 0
      emit(@repeated_sequence * @factor)
      @repeated_sequence = []
      transition_to(:default)
    else
      stay
    end
  end

  def to_s
    run.join
  end

  def size
    to_s.size
  end
end

class ExpanderV1 < ExpanderBase
  def initialize(input)
    super
    @factors = []
  end

  def step(char)
    @factors.each_index { |i| @factors[i][1] -= 1 }
    @factors.select! { |factor| factor[1] >= 0 }
    super
  end

  def finish_marker(factor, length)
    @factors.push([factor, length])
    transition_to(:default)
  end

  def finish_default(char)
    emit(@factors.map(&:first).inject(1, &:*))
    stay
  end

  def size
    run.sum
  end
end

class PartsBase
  attr_reader :input

  def initialize
    @input = IO.read("input.txt")
  end

  def run(part)
    puts "Part #{part}: #{result(part)}"
  end

  def runAll
    parts.keys.each(&method(:run))
  end
end

class PartsDay09 < PartsBase
  def parts
    {
      "One" => ExpanderV2,
      "Two" => ExpanderV1,
    }
  end

  def result(part)
    parts[part].new(@input).size
  end
end

if __FILE__ == $PROGRAM_NAME
  PartsDay09.new.runAll
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

  describe ExpanderV1 do
    describe :examples do
      it { expect(ExpanderV1.new("ADVENT").size).to eq(6) }
      it { expect(ExpanderV1.new("A(1x5)BC").size).to eq(7) }
      it { expect(ExpanderV1.new("X(8x2)(3x3)ABCY").size).to eq(20) }
      it { expect(ExpanderV1.new("(27x12)(20x12)(13x14)(7x10)(1x12)A").size).to eq(241920) }
      it { expect(ExpanderV1.new("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN").size).to eq(445) }

      it 'ignores whitespace' do
        expect(ExpanderV1.new("ADV\nENT").size).to eq(6)
      end
    end
  end

  describe PartsDay09 do
    describe "One" do
      subject { PartsDay09.new.result("One") }
      it { is_expected.to eq(99145) }
    end

    describe "Two" do
      subject { PartsDay09.new.result("Two") }
      it { is_expected.to eq(10943094568) }
    end
  end
end
