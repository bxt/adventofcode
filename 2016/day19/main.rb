
def last_elf_standing(elf_count)
  elves = elf_count.times.to_a
  index = 0

  loop do
    current_elf = elves.shift
    if elves.any?
      steal_from = yield elves
      # puts "#{steal_from + 1} ist raus, #{elves.size} übrig"
      elves.push(current_elf)
    else
      return current_elf + 1
    end
  end
end

input = 3001330 # --> 1808357 / 1407007
# input = 5

part_one = last_elf_standing(input) do |elves|
  elves.shift
end
puts "Part One: #{part_one}"

part_two = last_elf_standing(input) do |elves|
  across = (elves.size / 2.0).ceil - 1
  elves.delete_at(across)
end
puts "Part Two: #{part_two}"
