
def sup_ssl(ip)
  in_brackets = false
  supHas = false
  subOk = true
  abasIn = []
  abasOut = []
  last = []
  ip.strip.chars.each do |z|
    x, y = last
    case z
    when "["
      in_brackets = true
    when "]"
      in_brackets = false
    when x
      if z != y && !"[]".include?(y)
        if in_brackets
          abasIn.push [z,y]
        else
          abasOut.push [y,z]
        end
      end
    end
    last = [y,z]
  end
  puts "in"
  puts abasIn
  puts "out"
  puts abasOut
  match = abasIn & abasOut
  puts "match"
  puts match
  match.size > 0
end

result = IO.readlines("input.txt").map do |line|
  if sup_ssl(line)
    1
  else
    0
  end
end.inject(:+)

puts result
