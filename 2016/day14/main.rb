require 'digest/md5'

def find_repeated_char(times, string)
  return unless string
  matches = string.match(/(.)\1{#{times - 1}}/)
  return unless matches
  matches.captures[0]
end

def stretched_md5(salt, input)
  2017.times.reduce("#{salt}#{input}") do |prev, _|
    Digest::MD5.hexdigest(prev)
  end
end

def salted_md5(salt, input)
  Digest::MD5.hexdigest("#{salt}#{input}")
end

class KeyFinder
  def initialize(key_stream)
    @key_stream = key_stream
  end

  def print_key_at(index)
    max_index_diff = 999
    max_stream_index = 333333333
    found = []

    i = 0
    while i < max_stream_index
      candidate = @key_stream[i]
      if repeaded_char = find_repeated_char(5, candidate)
        (i - 1).downto(0).first(max_index_diff).each do |k|
          if find_repeated_char(3, @key_stream[k]) == repeaded_char
            found.push(k)
            print k
            print "\r"
            max_stream_index = i + max_index_diff + 1 if found.size == 64
          end
        end
      end
      i += 1
    end

    puts found.sort[index]
  end
end

class StretchedMD5KeyStream
  CHUNK_COUNT = 3
  CHUNK_SIZE = 8000

  def initialize(salt, &block)
    @salt = salt
    @precalculated_until = 0
    @store = {}
    @filter = block
  end

  def [](index)
    calculate_chunks if index >= @precalculated_until
    @store[index]
  end

  def calculate_chunks
    read_pipes = CHUNK_COUNT.times.map do |i|
      parent_read, child_write = IO.pipe
      fork do
        parent_read.close
        from = @precalculated_until + i*CHUNK_SIZE
        puts "> #{from}"
        result = {}
        CHUNK_SIZE.times do |k|
          index = from + k
          hash = stretched_md5(@salt, index)
          result[index] = hash if @filter.call(hash)
        end
        Marshal.dump(result, child_write)
        exit!(0)
      end
      child_write.close
      parent_read
    end

    Process.waitall

    read_pipes.each do |read_pipe|
      result = read_pipe.read
      result_hash = Marshal.load(result)
      @store.merge!(result_hash)
    end

    @precalculated_until += CHUNK_COUNT*CHUNK_SIZE
  end
end

salt = "abc" # -> 22728 / 22551
salt = "yjdafjpo" # -> 25427 / 22045

key_streams = {
  "One" => Hash.new { |h, i| h[i] = salted_md5(salt, i) if i >= 0 },
  "Two" => StretchedMD5KeyStream.new(salt) { |hash| find_repeated_char(3, hash) },
}

key_streams.each do |label, key_stream|
  puts "Part #{label}:"
  KeyFinder.new(key_stream).print_key_at(63)
end
