require 'digest/md5'

# key = "abc" # -> 22728
key = "yjdafjpo"

last1k = []
found = []
lastIndex = 333333333

i = 0
while i < lastIndex
  candidate = Digest::MD5.hexdigest("#{key}#{i}")
  if m = candidate.match(/(.)\1\1\1\1/)
    repeaded_letter = m.captures[0]
    key_indices = last1k.each_with_index.find_all do |hash, key_index|
      hash.match(/#{repeaded_letter}#{repeaded_letter}#{repeaded_letter}/)
    end.each do |hash, key_index|
      found.push(i - key_index - 1)
      puts i, key_index, i - key_index - 1, candidate, hash, repeaded_letter, '####'
      if found.size == 64
        lastIndex = i# + 10000
      end
    end
  end
  last1k.unshift(candidate)
  last1k.pop while last1k.size > 1000
  i += 1
end

puts found.size
puts found.sort[63]
puts found.sort.inspect
puts found.sort.last
