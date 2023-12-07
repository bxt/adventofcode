def solve(part)
  key = part == 1 ? '23456789TJQKA' : 'J23456789TQKA'

  hands = IO.readlines('input.txt').map do |line|
    hand, bid = line.split

    tallied = hand.chars.tally
    counts = tallied.values.sort.reverse

    jokers = tallied['J']
    if part == 2 && !jokers.nil? && counts != [jokers]
      counts.delete_at(counts.index(jokers))
      counts[0] += jokers
    end

    values = hand.chars.map { |c| key.index(c) }

    [[counts, values], bid.to_i]
  end

  hands.sort.map.with_index { |(_, bid), rank| bid * (rank + 1) }.sum
end

[1, 2].each { |part| puts "Part #{part}: #{solve(part)}" }
