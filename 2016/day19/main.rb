
input = 3001330 # --> 1808357
# input = 5

elves = input.times.to_a

index = 0

loop do
  current_elf = elves.shift
  if elves.any?
    steal_from = elves.shift
    # puts "#{steal_from + 1} ist raus"
    elves.push(current_elf)
  else
    puts "Elf #{current_elf + 1} gewinnt!"
    break
  end
end
