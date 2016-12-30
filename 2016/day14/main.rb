require 'digest/md5'
require 'optparse'

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
            max_stream_index = i + max_index_diff + 1 if found.size == index + 1
          end
        end
      end
      i += 1
    end

    puts found.sort[index]
  end
end

class BasicKeyStream
  def initialize(salt, &block)
    @salt = salt
    @store = Hash.new { |h, i| h[i] = @hash_function.call(@salt, i) if i >= 0 }
    @hash_function = block
  end

  def [](index)
    @store[index]
  end
end

class ParallelKeyStream
  CHUNK_COUNT = 20
  CHUNK_SIZE = 100

  def initialize(salt, &block)
    @salt = salt
    @precalculated_until = 0
    @store = {}
    @hash_function = block
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
        # puts "> #{from}"
        result = {}
        CHUNK_SIZE.times do |k|
          index = from + k
          result[index] = @hash_function.call(@salt, index)
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
  "One" => BasicKeyStream,
  "Two" => BasicKeyStream,
}

hash_functions = {
  "One" => method(:salted_md5),
  "Two" => method(:stretched_md5),
}

OptionParser.new do |opts|
  opts.banner = "Usage: main.rb [options]"

  opts.on('-c', '--use-native-extension', 'Enable to use native C extension for part two') do |v|
    require_relative './stretched_md5/stretched_md5'

    hash_functions["Two"] = Proc.new do |salt, i|
      StretchedMd5.get("#{salt}#{i}") if i >= 0
    end
  end

  opts.on('-f', '--use-fork-parallelism', 'Use a Fork/Join method to run parallel computations for part two') do |v|
    key_streams["Two"] = ParallelKeyStream
  end
end.parse!

key_streams.each do |label, key_stream_class|
  puts "Part #{label}:"
  hash_function = hash_functions[label]
  key_stream = key_stream_class.new(salt, &hash_function)
  KeyFinder.new(key_stream).print_key_at(63)
end
