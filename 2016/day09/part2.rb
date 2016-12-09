
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
  cont = 0

  in_bace = false
  num = []
  len = 0
  coun = 0
  rep = []

  cs.each do |c|
    if /\s/ === c
      # nothing
    else
      case c
      when "("
        in_bace = true
      when "x"
        len = num.join.to_i
        num = []
      when /\d/
        if in_bace
          num.push(c)
        else
          coun += rep.map(&:first).inject(1, &:*)
        end
      when ")"
        in_bace = false
        times = num.join.to_i
        num = []
        rep.push([times, len])
      else
        # puts rep.inspect
        coun += rep.map(&:first).inject(1, &:*)
      end
      rep.each_with_index {|c, i| rep[i][1] -= 1 }
      rep.select! {|c| c[1] >= 0 }
    end
  end

  coun
end

if __FILE__ == $PROGRAM_NAME

  foo = IO.readlines("input.txt").join("\n")
  r = expand(foo)

  puts r
end

if defined? RSpec
  describe :expand do
    describe :examples do
      it {expect(expand("ADVENT")).to eq(6)}
      it {expect(expand("A(1x5)BC")).to eq(7)}
      it {expect(expand("X(8x2)(3x3)ABCY")).to eq(20)}
      it {expect(expand("(27x12)(20x12)(13x14)(7x10)(1x12)A")).to eq(241920)}
      it {expect(expand("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN")).to eq(445)}
      it {expect(expand("ADV\nENT")).to eq(6)}
    end
  end
end
