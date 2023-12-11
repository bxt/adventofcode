def ds(coords)
  coords.sort.each_cons(2).with_index.inject([0]) do |acc, ((a, b), index)|
    acc.push(acc.last + (yield b - a) * index.succ)
  end.sum
end

def distance_sum(coords, times)
  sum_fix = ds(coords) { |diff| [diff, 1].min }
  sum_exp = ds(coords) { |diff| [diff.pred, 0].max }
  sum_fix + sum_exp * times
end

def solve(times)
  dimensions = [[], []]
  IO.readlines('input.txt').each.with_index do |line, y|
    new_xs = line.chars.each_index.select { |i| line[i] == '#' }
    dimensions[0].concat(new_xs)
    dimensions[1].concat([y] * new_xs.size)
  end
  dimensions.map { |coords| distance_sum(coords, times) }.sum
end

[[1, 2], [2, 1_000_000]].each do |part, times|
  puts "Part #{part}: #{solve(times)}"
end
