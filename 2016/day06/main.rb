require_relative "../day04/frequencies"

signals = IO.readlines("input.txt").map do |line|
  line.strip.chars
end

column_frequencies = signals.transpose.map(&method(:frequencies))

column_values_by_frequency_ascending = column_frequencies.map do |frequencies|
 frequencies.to_a.sort_by(&:reverse).map(&:first)
end

parts = {"One" => :last, "Two" => :first}

parts.each do |part, aggregator|
  result = column_values_by_frequency_ascending.map(&aggregator)
  puts "Part #{part}: #{result.join}"
end
