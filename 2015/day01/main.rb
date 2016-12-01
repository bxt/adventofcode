File.open("input.txt", "r") do |file|
  floor = 0
  basementAfter = 0
  searchingBasement = true

  while c = file.getc do
    floor += if c == '(' then 1 else -1 end
    if searchingBasement
      basementAfter += 1
      searchingBasement = floor != -1;
    end
  end

  puts "Part 1: #{floor}\nPart 2: #{basementAfter}\n"

end
