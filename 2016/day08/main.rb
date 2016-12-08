
class Screen
  CHARS = {on: '#', off: ' '}

  attr_accessor :field

  def initialize(width, height)
    @field = width.times.map { height.times.map { CHARS[:off] } }
  end

  def paint_rect(width, height)
    width.times do |x|
      height.times do |y|
        field[x][y] = CHARS[:on]
      end
    end
  end

  def rotate_row(y, by)
    tmp = field.transpose
    tmp[y].rotate!(-by)
    @field = tmp.transpose
  end

  def rotate_column(x, by)
    field[x].rotate!(-by)
  end

  def count(status)
    field.map { |col| col.count {|c| c == CHARS[status]} }.inject(:+)
  end

  def to_s
    field.transpose.map { |row| row.join }.join("\n")
  end
end

class InstructionParser
  def self.parse(lines)
    lines.map do |line|
      command = line.strip
      operands = line.match(/(\d+).*?(\d+)/).captures.map(&:to_i)
      case command
      when /^rect/
        [:paint_rect, operands]
      when /^rotate row/
        [:rotate_row, operands]
      when /^rotate column/
        [:rotate_column, operands]
      end
    end
  end

  def self.run(screen, instruction_ast)
    instruction_ast.each do |(instruction, operands)|
      screen.send(instruction, *operands)
    end
  end

  def self.parse_and_run(screen, lines)
    run(screen, parse(lines))
  end
end

# This file may be run by ruby or rspec

if __FILE__ == $PROGRAM_NAME
  screen = Screen.new(50, 6)
  InstructionParser.parse_and_run(screen, IO.readlines("input.txt"))
  puts "Part One: #{screen.count(:on)}"
  puts "Part Two: \n#{screen}"
end

if defined? RSpec
  describe Screen do
    subject { Screen.new(5, 4) }

    describe "#field" do
      it 'starts empty' do
        expect(subject.field).to eq([[' ',' ',' ',' '],[' ',' ',' ',' '],[' ',' ',' ',' '],[' ',' ',' ',' '],[' ',' ',' ',' ']])
      end
    end

    describe "#count" do
      it 'starts empty' do
        expect(subject.count(:on)).to eq(0)
        expect(subject.count(:off)).to eq(20)
      end

      it 'counts additions' do
        subject.field[1][2] = '#'
        expect(subject.count(:on)).to eq(1)
        expect(subject.count(:off)).to eq(19)
      end
    end

    describe "#to_s" do
      it 'can display empty pixel' do
        expect(subject.to_s).to eq("     \n     \n     \n     ")
      end

      it 'can display a single active pixel' do
        subject.field[2][3] = '#'
        expect(subject.to_s).to eq("     \n     \n     \n  #  ")
      end
    end

    describe '#paint_rect' do
      it 'can draw a rectangle' do
        subject.paint_rect(3, 2)
        expect(subject.count(:on)).to eq(6)
        expect(subject.to_s).to eq("###  \n###  \n     \n     ")
      end
    end

    describe '#rotate_row(y, by)' do
      it 'rotates the row y right by by pixels' do
        subject.field[1][0] = '#'
        subject.rotate_row(0, 2)
        expect(subject.field[3][0]).to eq('#')
      end

      it 'does not change the number of active pixels' do
        subject.field[1][0] = '#'
        subject.rotate_row(0, 2)
        expect(subject.count(:on)).to eq(1)
      end

      it 'leaves other rows unchanged' do
        subject.field[1][1] = '#'
        subject.rotate_row(0, 2)
        expect(subject.field[1][1]).to eq('#')
      end
    end

    describe '#rotate_column(x, by)' do
      it 'rotates the column x down by by pixels' do
        subject.field[1][0] = '#'
        subject.rotate_column(1, 2)
        expect(subject.field[1][2]).to eq('#')
      end

      it 'does not change the number of active pixels' do
        subject.field[1][0] = '#'
        subject.rotate_column(1, 2)
        expect(subject.count(:on)).to eq(1)
      end

      it 'leaves other columns unchanged' do
        subject.field[0][0] = '#'
        subject.rotate_column(1, 2)
        expect(subject.field[0][0]).to eq('#')
      end
    end

    it "runs the example" do
      screen = Screen.new(7, 3)
      expect(screen.to_s).to eq("       \n       \n       ")
      screen.paint_rect(3, 2)
      expect(screen.to_s).to eq("###    \n###    \n       ")
      screen.rotate_column(1, 1)
      expect(screen.to_s).to eq("# #    \n###    \n #     ")
      screen.rotate_row(0, 4)
      expect(screen.to_s).to eq("    # #\n###    \n #     ")
      screen.rotate_column(1, 1)
      expect(screen.to_s).to eq(" #  # #\n# #    \n #     ")
    end
  end

  describe InstructionParser do
    describe "#parse" do
      subject { InstructionParser.parse("  rotate row y=0 by 2 \n rect 1x2 \n rotate column x=32 by 1 ".lines) }
      it { is_expected.to eq([[:rotate_row, [0, 2]], [:paint_rect, [1, 2]], [:rotate_column, [32, 1]]]) }
    end

    describe "#run" do
      it 'runs a few command' do
        screen = double("screen")
        expect(screen).to receive(:rotate_row).with(0, 2).ordered
        expect(screen).to receive(:paint_rect).with(1, 2).ordered
        expect(screen).to receive(:rotate_column).with(32, 1).ordered
        InstructionParser.run(screen, [[:rotate_row, [0, 2]], [:paint_rect, [1, 2]], [:rotate_column, [32, 1]]])
      end
    end
  end
end
