file = File.read("input.txt")

i = file.split(", ")

dirs = [[1, 0], [0, 1], [-1, 0], [0, -1]]

cdir = 0

da = {"R" => 1, "L" => -1}
di = 0

st = [0, 0]

res = i.each do |p|
  x, y = st
  dp = da[p[0]]
  puts "-#{p[0]}-"
  if dp
    di += dp
    di = di % 4
    xo, yo = dirs[di]
    kk = p[1..-1].to_i
    st = [xo*kk + x, yo*kk + y]
  end
end.to_a

puts st
puts st.sum
