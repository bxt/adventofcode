
def sup_tls(ip)
  in_brackets = false
  supHas = false
  subOk = true
  last = []
  ip.strip.chars.each do |z|
    w, x, y = last
    case z
    when "["
      in_brackets = true
    when "]"
      in_brackets = false
    when w
      if x == y && z != x
        if in_brackets
          subOk = false
        else
          supHas = true
        end
      end
    end
    last = [x,y,z]
  end
  puts [subOk, supHas]
  subOk && supHas
end

result = IO.readlines("input.txt").map do |line|
  if sup_tls(line)
    1
  else
    0
  end
end.inject(:+)

puts result
