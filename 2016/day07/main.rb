
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
    match = abas_inside & abas_outside
    match.size > 0
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
    describe :supports_tls? do
      context "with supporting" do
        ["abba[mnop]qrst", "ioxxoj[asdfgh]zxcvbn"].each do |ip|
          it "reports true for #{ip}" do
            expect(Ipv7Address.new(ip).supports_tls?).to be_truthy
          end
        end
      end

      context "with non-supporting" do
        ["abcd[bddb]xyyx", "aaaa[qwer]tyui"].each do |ip|
          it "reports false for #{ip}" do
            expect(Ipv7Address.new(ip).supports_tls?).to be_falsey
          end
        end
      end
    end

    describe :supports_ssl? do
      context "with supporting" do
        ["aba[bab]xyz", "aaa[kek]eke", "zazbz[bzb]cdb"].each do |ip|
          it "reports true for #{ip}" do
            expect(Ipv7Address.new(ip).supports_ssl?).to be_truthy
          end
        end
      end

      context "with non-supporting" do
        ["xyx[xyx]xyx", "bwzsacxgqkbjycgfw[dbnligvrmqscasutn]rbgybqqsgjvlonkut"].each do |ip|
          it "reports false for #{ip}" do
            expect(Ipv7Address.new(ip).supports_ssl?).to be_falsey
          end
        end
      end
    end
  end

  RSpec.describe :parts do
    parts_result = parts

    describe :one do
      it "is 118" do
        expect(parts_result[:one]).to eq(118)
      end
    end

    describe :two do
      it "is 260" do
        expect(parts_result[:two]).to eq(260)
      end
    end
  end
end
