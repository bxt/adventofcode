
input = 3001330 # --> 1407007
# input = 5

elves = input.times.to_a

index = 0

loop do
  current_elf = elves.shift
  if elves.any?
    across = (elves.size / 2.0).ceil - 1
    steal_from = elves.delete_at(across)
    puts "#{steal_from + 1} ist raus, #{elves.size} übrig"
    elves.push(current_elf)
  else
    puts "Elf #{current_elf + 1} gewinnt!"
    break
  end
end
