
class Ipv7Address
  attr_accessor :ip_string

  def initialize(ip_string)
    @ip_string = ip_string.strip
  end

  def supports_tls?
    has_abba_outside = false
    no_abba_inside = true
    each_cons_with_bracket_state(4) do |w, x, y, z, in_brackets|
      if [w, x] == [z, y] && z != y
        if in_brackets
          no_abba_inside = false
        else
          has_abba_outside = true
        end
      end
      #puts "state", w, x, y, z, in_brackets, has_abba_inside, no_abba_outside
    end
    has_abba_outside && no_abba_inside
  end

  def supports_ssl?
    abas_inside = []
    abas_outside = []
    each_cons_with_bracket_state(3) do |x, y, z, in_brackets|
      if x == z && z != y
        if in_brackets
          abas_inside.push [z,y]
        else
          abas_outside.push [y,z]
        end
      end
    end
    abas_in_both = abas_inside & abas_outside
    abas_in_both.any?
  end

  private

  BRACKETS = {open: "[", close: "]"}

  def each_cons_with_bracket_state(n)
    in_brackets = false
    ip_string.chars.each_cons(n) do |char_list|
      case char_list.last
      when BRACKETS[:open]
        in_brackets = true
      when BRACKETS[:close]
        in_brackets = false
      else
        if (char_list & BRACKETS.values).empty?
          yield *char_list.push(in_brackets)
        end
      end
    end
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
