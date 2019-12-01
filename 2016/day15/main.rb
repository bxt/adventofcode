
DISC_PATTERN = /Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+).$/

discs = IO.readlines("input.txt").map do |line|
  id, max, start = line.match(DISC_PATTERN).captures
  [id.to_i, max.to_i, start.to_i]
end

# discs = [[5,4],[2,1]]

def find_good_time(discs)
  (1..).find do |time|
    discs.all? do |disc_no, max, start|
      (time + disc_no + start) % max == 0
    end
  end
end

puts "Part One: #{find_good_time(discs)}"
puts "Part Two: #{find_good_time(discs + [[7, 11, 0]])}"
