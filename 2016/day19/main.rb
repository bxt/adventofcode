
input = 3001330 # --> 1808357
# input = 5

elves = input.times.reverse_each.to_a

index = 0

loop do
  current_elf = elves.pop
  if elves.any?
    steal_from = elves.pop
    # puts "#{steal_from + 1} ist raus"
    elves.unshift(current_elf)
  else
    puts "Elf #{current_elf + 1} gewinnt!"
    break
  end
end
