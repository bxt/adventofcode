rooms = IO.readlines("input.txt").map do |line|
  sector_id_sting, checksum = line.match(/(\d+)\[([a-z]+)\]$/).captures
  sector_id = sector_id_sting.to_i
  name_parts = line.scan(/(([a-z]+)-)/).map{ |match_data| match_data[1] }
  [sector_id, checksum, name_parts]
end

valid_rooms = rooms.find_all do |(sector_id, givenChecksum, name_parts)|
  all_letters = name_parts.reduce(:+).chars.sort

  letter_counts = all_letters.each_with_object(Hash.new(0)) do |letter,counts|
    counts[letter] += 1
  end
  letters_by_frequency = letter_counts.sort_by{|c|[-c[1],c[0]]}
  most_frequent_letters = letters_by_frequency.take(5).map(&:first)
  actualChecksum = most_frequent_letters.join

  actualChecksum == givenChecksum
end

valid_checksum_sum = valid_rooms.map(&:first).reduce(:+)

ALPHABET = "abcdefghijklmnopqrstuvwxyz"

def rot(string, n)
  string.chars.map do |letter|
    ALPHABET[(ALPHABET.index(letter) + n) % ALPHABET.size]
  end.join
end

decrypted_rooms = valid_rooms.map do |(sector_id, givenChecksum, name_parts)|
  decrypted_name_parts = name_parts.map do |name_part|
    rot(name_part, sector_id)
  end
  [sector_id, givenChecksum, decrypted_name_parts]
end

north_pole_room = decrypted_rooms.find do |(sector_id, givenChecksum, decrypted_name_parts)|
  decrypted_name_parts.join(' ') == "northpole object storage"
end

id_of_north_pole_room = north_pole_room.first

puts "PartOne: #{valid_checksum_sum}"
puts "PartTwo: #{id_of_north_pole_room}"
