
c = IO.readlines("input.txt").map do |line|
  line.chars
end

r = c[0].each_index.map do |i|
  letter_counts = c.map{|a|a[i]}.each_with_object(Hash.new(0)) do |letter,counts|
    counts[letter] += 1
  end
  letters_by_frequency = letter_counts.sort_by do |letter, count|
    [count, letter]
  end
  most_frequent_letter = letters_by_frequency.first.first
  most_frequent_letter
end

puts r.join
