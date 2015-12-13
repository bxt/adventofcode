require 'active_support/core_ext/enumerable'

def process_file file, *regexes
  IO.readlines(file).count do |line|
    regexes.all? {|r| r =~ line}
  end
end

threeVowels  = /([aeiou].*){3}/
doubleLetter = /(.)\1/
blacklist    = /^((?!ab|cd|pq|xy).)*$/
doublePair   = /(..).*\1/
withBetween  = /(.).\1/

puts "part1: #{process_file("input.txt", threeVowels, doubleLetter, blacklist)}"
puts "part2: #{process_file("input.txt", doublePair, withBetween)}"
