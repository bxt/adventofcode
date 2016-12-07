
class DelimitedChunks
  attr_accessor :delimiters, :chunk_size

  def initialize(delimiters, chunk_size)
    @delimiters = delimiters
    @chunk_size = chunk_size
  end

  def collect_inside_outside(array)
    inside = []
    outside = []
    is_inside = false
    array.each_cons(chunk_size) do |chunk|
      case chunk.last
      when delimiters[:open]
        is_inside = true
      when delimiters[:close]
        is_inside = false
      else
        if (chunk & delimiters.values).empty?
          if is_inside
            inside.push(chunk)
          else
            outside.push(chunk)
          end
        end
      end
    end
    [inside, outside]
  end
end

class Ipv7Address
  attr_accessor :ip_string

  def initialize(ip_string)
    @ip_string = ip_string.strip
  end

  def supports_tls?
    abbas_inside, abbas_outside = chunks(4).map do |chunk_list|
      chunk_list.find_all do |w, x, y, z|
        [w, x] == [z, y] && z != y
      end
    end
    abbas_outside.any? && abbas_inside.none?
  end

  def supports_ssl?
    abas_inside, abas_outside = chunks(3).map do |chunk_list|
      chunk_list.find_all do |x, y, z|
        x == z && z != y
      end
    end
    abas_in_both = abas_inside & abas_outside.map{ |x, y, z| [y, x, y] }
    abas_in_both.any?
  end

  private

  BRACKETS = {open: "[", close: "]"}

  def chunks(chunk_size)
    DelimitedChunks.new(BRACKETS, chunk_size).collect_inside_outside(ip_string.chars)
  end
end

def parts
  ipv7_addresses = IO.readlines("input.txt").map(&Ipv7Address.method(:new))
  {
    one: ipv7_addresses.count(&:supports_tls?),
    two: ipv7_addresses.count(&:supports_ssl?),
  }
end

# This file may be run by ruby or rspec

if __FILE__ == $PROGRAM_NAME
  parts.each do |part, result|
    puts "Part #{part}: #{result}"
  end
end

if defined? RSpec
  RSpec.describe DelimitedChunks do
    context "with numbers" do
      subject(:instance) { DelimitedChunks.new({open: -1, close: -2}, 2) }
      describe :collect_inside_outside do
        subject(:result) { instance.collect_inside_outside([1,2,-1,3,4,-2,5,6,-1,7,8,9,-2,10]) }

        describe "first result (inside)" do
          it "collects elements" do
            expect(result[0]).to match_array([[3,4],[7,8],[8,9]])
          end
        end

        describe "second result (outside)" do
          it "collects elements" do
            expect(result[1]).to match_array([[1,2],[5,6]])
          end
        end
      end
    end

    context "with strings" do
      subject(:instance) { DelimitedChunks.new({open: "(", close: ")"}, 3) }
      describe :collect_inside_outside do
        subject(:result) { instance.collect_inside_outside("abc(fed)ghi".chars) }

        describe "first result (inside)" do
          it "collects elements" do
            expect(result[0]).to match_array(["fed".chars])
          end
        end

        describe "second result (outside)" do
          it "collects elements" do
            expect(result[1]).to match_array(["abc".chars, "ghi".chars])
          end
        end
      end
    end
  end

  RSpec.describe Ipv7Address do
    examples = {
      supports_tls?: {
        be_truthy: ["abba[mnop]qrst", "ioxxoj[asdfgh]zxcvbn"],
        be_falsey: ["abcd[bddb]xyyx", "aaaa[qwer]tyui"],
      },
      supports_ssl?: {
        be_truthy: ["aba[bab]xyz", "aaa[kek]eke", "zazbz[bzb]cdb"],
        be_falsey: ["xyx[xyx]xyx", "bwzsacxgqkbjycgfw[dbnligvrmqscasutn]rbgybqqsgjvlonkut"],
      },
    }
    examples.each do |method, results|
      describe method do
        results.each do |result, inputs|
          inputs.each do |input|
            describe input do
              subject { Ipv7Address.new(input).public_send(method) }
              it { is_expected.to public_send(result) }
            end
          end
        end
      end
    end
  end

  RSpec.describe :parts do
    subject { parts }
    it { is_expected.to include(one: 118) }
    it { is_expected.to include(two: 260) }
  end
end
