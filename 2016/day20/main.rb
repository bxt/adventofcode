
def merge_range_lists(a, b)
  result = []

  building = nil
  while a.any? || b.any?
    current = if !a.any? || (b.any? && b.first[:from] < a.first[:from])
      b.shift
    else
      a.shift
    end
    if !building || current[:from] > building[:to]
      result.push(building) if building
      building = current.dup
    else
      building[:to] = [building[:to], current[:to]].max
    end
  end

  result.push(building) if building
  result
end

def build_range_list(ranges)
  range_lists = ranges.map { |x| [x] }
  while range_lists.size > 1
    range_lists = range_lists.each_slice(2).map do |a, b|
      if b
        merge_range_lists(a, b)
      else
        a
      end
    end
  end
  range_lists.first
end

def range_size(range)
  range[:to] - range[:from]
end

def range_list_size(range_list)
  range_list.map(&method(:range_size)).inject(:+)
end

ip_ranges = IO.readlines("input.txt").map do |line|
  from, to = line.strip.split("-")
  {from: from.to_i, to: to.to_i + 1} # convert inclusive to exlusive
end
ip_ranges = build_range_list(ip_ranges)

puts "Part One: #{ip_ranges[0][:to]}"

total_count = 4294967295 + 1
blocked_count = range_list_size(ip_ranges)
puts "Part Two: #{total_count - blocked_count}"
