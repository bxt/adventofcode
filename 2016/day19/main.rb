
input = 3001330 # --> 1808357
#input = 5

elves = [true] * input

index = 0

loop do
  while !elves[index]
    index = (index + 1) % elves.size
  end
  steal_offset = (1...elves.size).find do |i|
    elves[(index + i) % elves.size]
  end
  steal_from = (index + steal_offset) % elves.size if steal_offset
  if steal_from
    puts "#{steal_from + 1} ist raus"
    elves[steal_from] = false
    index += 1
  else
    puts "Elf #{index + 1} gewinnt!"
    break
  end
end
