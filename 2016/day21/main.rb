
class PasswordScrambler
  attr_accessor :password

  REGEXES = {
    swap_position: /swap position (\d+) with position (\d+)/,
    swap_letter: /swap letter ([a-z]) with letter ([a-z])/,
    rotate: /rotate (left|right) (\d+) steps?/,
    special_rotate: /rotate based on position of letter ([a-z])/,
    reverse: /reverse positions (\d+) through (\d+)/,
    move: /move position (\d+) to position (\d+)/,
  }

  def initialize(initial)
    @password = initial
  end

  def scramble(instructions)
    instructions.each do |instruction|
      REGEXES.each do |method, regex|
        if m = instruction.match(regex)
          send(method, *m.captures)
        end
      end
    end
  end

  def unscramble(instructions)
    instructions.reverse_each do |instruction|
      REGEXES.each do |method, regex|
        if m = instruction.match(regex)
          send(:"un#{method}", *m.captures)
        end
      end
    end
  end

  def swap_position(*position_strings)
    position_one, position_two = position_strings.map(&:to_i)
    swap_letter(password[position_one], password[position_two])
  end

  def swap_letter(letter_one, letter_two)
    @password = password.tr(letter_one + letter_two, letter_two + letter_one)
  end

  def rotate_by(steps)
    @password = password.chars.rotate(steps).join
  end

  def rotate(direction, steps_s)
    direction_factor = {
      left: 1,
      right: -1,
    }[direction.to_sym]
    rotate_by(steps_s.to_i * direction_factor)
  end

  def special_rotate(letter)
    index = password.index(letter)
    rotate_by(-special_rotate_steps(index))
  end

  def reverse(*position_strings)
    position_one, position_two = position_strings.map(&:to_i)
    password[position_one..position_two] = password[position_one..position_two].reverse
  end

  def move(*position_strings)
    position_one, position_two = position_strings.map(&:to_i)
    letter = password.slice!(position_one)
    password.insert(position_two, letter)
  end

  alias_method :unswap_position, :swap_position
  alias_method :unswap_letter, :swap_letter
  alias_method :unreverse, :reverse

  def unrotate(direction, steps_s)
    direction_factor = {
      left: -1,
      right: 1,
    }[direction.to_sym]
    rotate_by(steps_s.to_i * direction_factor)
  end

  def unspecial_rotate(letter)
    index_after = password.index(letter)
    index_before = special_rotate_lookup_table[index_after]
    rotate_by(index_after - index_before)
  end

  def unmove(*position_strings)
    move(*position_strings.reverse)
  end

  def special_rotate_lookup_table
    @srlt ||= password.size.times.each_with_object({}) do |index_before, table|
      index_after = (index_before + special_rotate_steps(index_before)) % password.size
      table[index_after] = index_before
    end
  end

  def special_rotate_steps(index)
    extra = index >= 4 ? 2 : 1
    (index + extra) % password.size
  end
end

if __FILE__ == $PROGRAM_NAME
  password_scrambler = PasswordScrambler.new("abcdefgh")
  password_scrambler.scramble(IO.readlines("input.txt"))
  puts password_scrambler.password

  password_scrambler = PasswordScrambler.new("fbgdceah")
  password_scrambler.unscramble(IO.readlines("input.txt"))
  puts password_scrambler.password
end

if defined? RSpec
  describe PasswordScrambler do
    it 'scrambles the example' do
      password_scrambler = described_class.new("abcde")
      expect(password_scrambler.password).to eq("abcde")

      password_scrambler.scramble(["swap position 4 with position 0"])
      expect(password_scrambler.password).to eq("ebcda")

      password_scrambler.scramble(["swap letter d with letter b"])
      expect(password_scrambler.password).to eq("edcba")

      password_scrambler.scramble(["reverse positions 0 through 4"])
      expect(password_scrambler.password).to eq("abcde")

      password_scrambler.scramble(["rotate left 1 step"])
      expect(password_scrambler.password).to eq("bcdea")

      password_scrambler.scramble(["move position 1 to position 4"])
      expect(password_scrambler.password).to eq("bdeac")

      password_scrambler.scramble(["move position 3 to position 0"])
      expect(password_scrambler.password).to eq("abdec")

      password_scrambler.scramble(["rotate based on position of letter b"])
      expect(password_scrambler.password).to eq("ecabd")

      password_scrambler.scramble(["rotate based on position of letter d"])
      expect(password_scrambler.password).to eq("decab")
    end

    it 'unscrambles the example' do
      password_scrambler = described_class.new("decab")
      expect(password_scrambler.password).to eq("decab")

      password_scrambler.unscramble(["rotate based on position of letter d"])
      expect(password_scrambler.password).to eq("ecabd")

      password_scrambler.unscramble(["rotate based on position of letter b"])
      expect(password_scrambler.password).to eq("abdec")

      password_scrambler.unscramble(["move position 3 to position 0"])
      expect(password_scrambler.password).to eq("bdeac")

      password_scrambler.unscramble(["move position 1 to position 4"])
      expect(password_scrambler.password).to eq("bcdea")

      password_scrambler.unscramble(["rotate left 1 step"])
      expect(password_scrambler.password).to eq("abcde")

      password_scrambler.unscramble(["reverse positions 0 through 4"])
      expect(password_scrambler.password).to eq("edcba")

      password_scrambler.unscramble(["swap letter d with letter b"])
      expect(password_scrambler.password).to eq("ebcda")

      password_scrambler.unscramble(["swap position 4 with position 0"])
      expect(password_scrambler.password).to eq("abcde")
    end

    it 'calls methods and passes parameters' do
      password_scrambler = described_class.new("abcde")

      expect(password_scrambler).to receive(:swap_position).with("4", "0").and_return(nil)
      expect(password_scrambler).to receive(:swap_letter).with("d", "b").and_return(nil)
      expect(password_scrambler).to receive(:reverse).with("0", "4").and_return(nil)
      expect(password_scrambler).to receive(:rotate).with("left", "1").and_return(nil)
      expect(password_scrambler).to receive(:move).with("1", "4").and_return(nil)
      expect(password_scrambler).to receive(:move).with("3", "0").and_return(nil)
      expect(password_scrambler).to receive(:special_rotate).with("b").and_return(nil)
      expect(password_scrambler).to receive(:special_rotate).with("d").and_return(nil)

      password_scrambler.scramble([
        "swap position 4 with position 0",
        "swap letter d with letter b",
        "reverse positions 0 through 4",
        "rotate left 1 step",
        "move position 1 to position 4",
        "move position 3 to position 0",
        "rotate based on position of letter b",
        "rotate based on position of letter d",
      ])
    end
  end
end
