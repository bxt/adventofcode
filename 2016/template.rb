
def frequencies(array)
  array.each_with_object(Hash.new(0)) do |value, counts|
    counts[value] += 1
  end
end

result = IO.readlines("input.txt").map do |line|
  line.strip.chars
  a, b = line.match(/^(\d+) ([a-z]+)$/).captures
  parts = line.scan(/(([a-z]+)-)/).map{ |match_data| match_data[1] }




end

puts result
