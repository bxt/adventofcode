require_relative "frequencies"

ALPHABET = ("a".."z").to_a.join

def rot(string, n)
  n %= ALPHABET.size
  string.tr(ALPHABET, ALPHABET[n..-1] + ALPHABET[0...n])
end

class Room < Struct.new(:sector_id, :given_checksum, :name_parts)
  def self.parse(room_string)
    sector_id_sting, checksum = room_string.match(/(\d+)\[([a-z]+)\]$/).captures

    sector_id = sector_id_sting.to_i

    name_parts = room_string.scan(/(([a-z]+)-)/).map{ |match_data| match_data[1] }

    Room.new(sector_id, checksum, name_parts)
  end

  def valid?
    given_checksum == actual_checksum
  end

  def decrypted_name
    decrypted_name_parts.join(' ')
  end

  def decrypted_name_parts
    name_parts.map(&method(:decrypt_name_part))
  end

  private

  def actual_checksum
    all_letters = name_parts.reduce(:+).chars

    letters_by_frequency = frequencies(all_letters).sort_by do |letter, count|
      [-count, letter]
    end
    most_frequent_letters = letters_by_frequency.take(5).map(&:first)

    most_frequent_letters.join
  end

  def decrypt_name_part(name_part)
    rot(name_part, sector_id)
  end
end

rooms = IO.readlines("input.txt").map(&Room.method(:parse))
valid_rooms = rooms.find_all(&:valid?)

valid_checksum_sum = valid_rooms.map(&:sector_id).sum

north_pole_room = valid_rooms.find do |room|
  room.decrypted_name == "northpole object storage"
end

puts "PartOne: #{valid_checksum_sum}"
puts "PartTwo: #{north_pole_room.sector_id}"
