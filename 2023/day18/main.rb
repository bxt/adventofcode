[1, 2].each do |part|
  trench_size, lagoon_size, x_position = (0..).take(3)
  IO.readlines('input.txt').each do |line|
    /(?<dp1>.) (?<lp1>\d+) \(#(?<lp2>\h{5})(?<dp2>\h)\)/ =~ line
    length, dir = part == 1 ? [lp1.to_i, 'RDLU'.index(dp1)] : [lp2.to_i(16), dp2.to_i]
    trench_size += length
    lagoon_size += [0, -1, 0, 1][dir] * length * x_position
    x_position += [-1, 0, 1, 0][dir] * length
  end
  puts "Part #{part}: #{lagoon_size + trench_size / 2}"
end
