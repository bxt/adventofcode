
def frequencies(array)
  array.each_with_object(Hash.new(0)) do |value, counts|
    counts[value] += 1
  end
end

# 809547 too high
# 809547
# 99145

def expand(foo)
  cs = foo.chars
  o = []

  in_bace = false
  in_rep = false
  num = []
  len = 0
  coun = 0
  rep = []

  cs.each do |c|
    if /\s/ === c
      # nothing
    else
      if in_rep
        if len > 0
          len -= 1
          rep.push(c)
        end
        if len <= 0
          in_rep = false
          o.push(rep * coun)
          len = 0
          rep = []
        end
      else
        case c
        when /\s/
          # nothing
        when "("
          in_bace = true
        when "x"
          len = num.join.to_i
          num = []
        when /\d/
          if in_bace
            num.push(c)
          else
            o.push(c)
          end
        when ")"
          in_bace = false
          in_rep = true
          coun = num.join.to_i

          num = []
        else
          o.push(c)
        end
      end
    end
  end

  o.join
end

if __FILE__ == $PROGRAM_NAME
  compressed = IO.read("input.txt")
  uncompressed = expand(compressed)
  puts uncompressed.size
end

if defined? RSpec
  describe :expand do
    describe :examples do
      it {expect(expand("ADVENT")).to eq("ADVENT")}
      it {expect(expand("A(1x5)BC")).to eq("ABBBBBC")}
      it {expect(expand("(3x3)XYZ")).to eq("XYZXYZXYZ")}
      it {expect(expand("A(2x2)BCD(2x2)EFG")).to eq("ABCBCDEFEFG")}
      it {expect(expand("(6x1)(1x3)A")).to eq("(1x3)A")}
      it {expect(expand("X(8x2)(3x3)ABCY")).to eq("X(3x3)ABC(3x3)ABCY")}
      it {expect(expand("ADV\nENT")).to eq("ADVENT")}
    end
  end
end
