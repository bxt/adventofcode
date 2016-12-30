def frequencies(array)
  array.each_with_object(Hash.new(0)) do |value, counts|
    counts[value] += 1
  end
end
