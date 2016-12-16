
def expand(string)
  string + "0" + string.reverse.tr('01','10')
end

def generate(string, length)
  while string.size < length
    string = expand(string)
  end
  string[0, length]
end

def shrink(string)
  string.gsub(/../) do |pair|
    pair[0] == pair[1] ? '1' : '0'
  end
end

def checksum(string)
  while string.size.even?
    string = shrink(string)
  end
  string
end

input = "10001110011110000"

puts "Example: #{checksum(generate("10000", 20))}"
puts "Part One: #{checksum(generate(input, 272))}"
puts "Part One: #{checksum(generate(input, 35651584))}"
