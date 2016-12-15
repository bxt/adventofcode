
discs = IO.readlines("input.txt").map do |line|
  max, start = line.match(/Disc #\d+ has (\d+) positions; at time=0, it is at position (\d+).$/).captures
  [max.to_i, start.to_i]
end

# discs = [[5,4],[2,1]]

SEARCHMAX = 1000000000

def find_good_time(discs)
  SEARCHMAX.times.find do |i|
    discs.each_with_index.all? do |(max, start), disc_no|
      (i + 1 + start + disc_no) % max == 0
    end
  end
end

puts "Part One: #{find_good_time(discs)}"
puts "Part Two: #{find_good_time(discs + [[11, 0]])}"
