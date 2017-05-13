# For Crystal 0.22.0 (2017-04-20) LLVM 4.0.0
# Run this: crystal build main.cr --release && /usr/bin/time ./main && rm main

require "digest/md5"
require "json"

def find_repeated_char(times, string)
  return unless string
  matches = string.match(/(.)\1{#{times - 1}}/)
  return unless matches
  matches[1]
end

def stretched_md5(salt : String, input : Int32) : String
  2017.times.reduce("#{salt}#{input}") do |prev, _|
    Digest::MD5.hexdigest(prev)
  end
end

def salted_md5(salt : String, input : Int32) : String
  Digest::MD5.hexdigest("#{salt}#{input}")
end

class KeyFinder
  def initialize(@key_stream : KeyStream)
  end

  def print_key_at(index)
    max_index_diff = 999
    # max_stream_index = 27427
    max_stream_index = 333333333
    found = [] of Int32

    i = 0
    while i < max_stream_index
      candidate = @key_stream[i]
      if repeaded_char = find_repeated_char(5, candidate)
        (i - 1).downto(0).first(max_index_diff).each do |k|
          if find_repeated_char(3, @key_stream[k]) == repeaded_char
            found.push(k)
            print k
            print "\r"
            max_stream_index = i + max_index_diff + 1 if found.size == index + 1
          end
        end
      end
      i += 1
    end

    puts found.sort[index]
  end
end

abstract class KeyStream
  abstract def [](index)
end

class BasicKeyStream < KeyStream
  def initialize(@salt : String, @hash_function : String, Int32 -> String)
    @store = Hash(Int32, String).new do |h, i|
      if i >= 0
        h[i] = @hash_function.call(@salt, i)
      else
        raise "Out of bounds: #{i}"
      end
    end
  end

  def [](index)
    @store[index]
  end
end

class ParallelKeyStream < KeyStream
  CHUNK_COUNT = 20
  CHUNK_SIZE = 100

  def initialize(@salt : String, @hash_function : String, Int32 -> String)
    @precalculated_until = 0
    @store = {} of Int32 => String
  end

  def [](index)
    calculate_chunks if index >= @precalculated_until
    @store[index]
  end

  def calculate_chunks
    chunks = CHUNK_COUNT.times.map do |i|
      parent_read, child_write = IO.pipe
      process = fork do
        parent_read.close
        from = @precalculated_until + i*CHUNK_SIZE
        result = {} of Int32 => String
        CHUNK_SIZE.times do |k|
          index = from + k
          result[index] = @hash_function.call(@salt, index)
        end
        JSON.build(child_write) do |json|
          result.to_json(json)
        end
        exit(0)
      end
      child_write.close
      {process, parent_read, "a#{i}"}
    end

    chunks.each do |(chunk, read_pipe, string)|
      chunk.wait unless chunk.terminated?
      JSON.parse(read_pipe).each do |k, v|
        @store[k.as_s.to_i32] = v.as_s
      end
    end

    @precalculated_until += CHUNK_COUNT*CHUNK_SIZE
  end
end

salt = "abc" # -> 22728 / 22551
salt = "yjdafjpo" # -> 25427 / 22045

parts = {
  "One" => BasicKeyStream.new(salt, ->salted_md5(String, Int32)),
  #"Two" => ParallelKeyStream.new(salt, ->stretched_md5(String, Int32)),
  "Two" => BasicKeyStream.new(salt, ->stretched_md5(String, Int32)),
}

parts.each do |label, key_stream|
  puts "Part #{label}:"
  KeyFinder.new(key_stream).print_key_at(63)
end
