require 'active_support/core_ext/enumerable'

def process_file file, regex, values
  IO.readlines(file).map do |line|
    line.scan(regex).map do |m|
      values[m.index{|x|!x.nil?}]
    end.sum + 2
  end.sum
end

puts "part1: #{process_file("input.txt", /(\\")|(\\\\)|(\\x[0-9a-f]{2})/, [1, 1, 3])}" 
puts "part2: #{process_file("input.txt", /(\\)|(")/, [1, 1])}"
