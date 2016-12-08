
def frequencies(array)
  array.each_with_object(Hash.new(0)) do |value, counts|
    counts[value] += 1
  end
end

field = 50.times.map { 6.times.map { '.' } }
# field = [".......".chars, ".......".chars, ".......".chars].transpose

commands = IO.readlines("input.txt")
# commands = ["rect 3x2","rotate column x=1 by 1", "rotate row y=0 by 4"]

commands.map do |line|
  command = line.strip
  operands = line.match(/(\d+).*?(\d+)$/).captures.map(&:to_i)
  case command
  when /^rotate row/
    y, by = operands
    tmp = field.transpose
    tmp[y].rotate!(-by)
    field = tmp.transpose
  when /^rect/
    width, height = operands
    width.times do |x|
      height.times do |y|
        field[x][y] = '#'
      end
    end
  when /^rotate column/
    x, by = operands
    field[x].rotate!(-by)
  end
end

field.transpose.each { |row| puts row.join }

puts field.map { |col| col.count {|c| c == '#'} }.inject(:+)

# rotate row y=0 by 2
# rect 1x2
# rotate column x=32 by 1
