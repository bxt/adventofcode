
input = "10001110011110000"
#len = 272
len = 35651584

#input = "10000"
#len = 20

def expand(string)
  string + "0" + string.reverse.tr('01','10')
end

while input.size < len
  input = expand(input)
  #puts "expanded to: #{input}"
end

input = input[0, len]

while input.size.even?
  input.gsub!(/../) do |cs|
    cs[0] == cs[1] ? '1' : '0'
  end
  #puts "smalled to: #{input}"
end

puts input
