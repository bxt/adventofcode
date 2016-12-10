
class Node
  attr_reader :id, :values

  def initialize(id, &emitter)
    @id = id
    @values = []
    @emitter = emitter
  end

  def emit(*args)
    @emitter.call(args)
  end

  def receive(value)
    @values.push(value)
    try_transition
  end

  def try_transition
    if ready?
      transit
      @values = []
    end
  end
end

class Output < Node
  def transit
    emit(:output_dump, id, values)
  end

  def ready?
    values.any?
  end
end

class Bot < Node
  attr_accessor :low, :high

  def transit
    values.sort!
    low_value, high_value = values
    emit(:bot_sort, id, low_value, high_value)
    low.receive(low_value)
    high.receive(high_value)
  end

  def ready?
    values.size == 2
  end
end

class Network
  attr_reader :bots, :outputs, :events

  def initialize
    @events = []
    @bots = Hash.new { |hash, key| hash[key] = Bot.new(key, &@events.method(:push)) }
    @outputs = Hash.new { |hash, key| hash[key] = Output.new(key, &@events.method(:push)) }
  end

  def get(type, id)
    case type
    when "bot"
      bots[id]
    when "output"
      outputs[id]
    end
  end

  BOT = /(bot) (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)/
  def parse_definitions(input)
    input.lines.each do |line|
      if BOT === line
        type, id, low_type, low_id, high_type, high_id = line.match(BOT).captures
        from = get(type, id)
        low = get(low_type, low_id)
        high = get(high_type, high_id)
        from.low = low
        from.high = high
      end
    end
  end

  FEED = /value (\d+) goes to (bot|output) (\d+)/
  def parse_feeds(input)
    input.lines.each do |line|
      if FEED === line
        value, to_type, to_id = line.match(FEED).captures
        to = get(to_type, to_id)
        to.receive(value.to_i)
      end
    end
  end
end

class PartsDay10
  def initialize
    input = IO.read("input.txt")
    @network = Network.new
    @network.parse_definitions(input)
    @network.parse_feeds(input)
  end

  def part_one
    @network.events.select do |e|
      e[0] == :bot_sort
    end.find do |(_, _, low, high)|
      [low, high] == [17, 61]
    end[1]
  end

  def part_two
    @network.events.select do |e|
      e[0] == :output_dump && ["0", "1", "2"].include?(e[1])
    end.map(&:last).map(&:first).inject(&:*)
  end

  def runAll
    ["One", "Two"].each do |part|
      puts "Part #{part}: #{send("part_#{part.downcase}")}"
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  PartsDay10.new.runAll
end

if defined?(RSpec)
  describe PartsDay10 do
    subject(:parts) { PartsDay10.new }

    describe "One" do
      subject { parts.part_one }
      it { is_expected.to eq("161") }
    end

    describe "Two" do
      subject { parts.part_two }
      it { is_expected.to eq(133163) }
    end
  end
end
