require 'active_support/core_ext/enumerable'
require 'chunky_png'

class Light
  def initialize(width, height)
    @width = width
    @height = height
    @lights = height.times.map {|x| width.times.map {|y| :off}}
  end

  def count_active_lights
    @lights.map { |line| line.count(:on) }.sum
  end

  def process_file file
    IO.readlines(file).each do |line|
      m = /(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)/.match(line)
      command, from_x, from_y, to_x, to_y = m[1..5]
      send(command.tr(" ", "_"), [from_x.to_i, from_y.to_i], [to_x.to_i, to_y.to_i])
    end
  end

  def each_light from, to
    (from[1]..to[1]).each do |y|
      (from[0]..to[0]).each do |x|
        @lights[y][x] = yield @lights[y][x]
      end
    end
  end

  def toggle from, to
    each_light from, to do |light|
      light == :on ? :off : :on
    end
  end

  def turn_on from, to
    each_light from, to do |light|
      :on
    end
  end

  def turn_off from, to
    each_light from, to do |light|
      :off
    end
  end

  def to_s
    @lights.map do |line|
      line.map do |light|
        light == :on ? "○" : "●"
      end.join(' ')
    end.join("\n")
  end

  def to_png file
    png = ChunkyPNG::Image.new(@width, @height, ChunkyPNG::Color::TRANSPARENT)
    (0...@height).each do |y|
      (0...@width).each do |x|
        png[x,y] = color(@lights[y][x])
      end
    end

    png.save(file)
  end

private

  COLORS = {on: ChunkyPNG::Color.rgba(255, 255, 100, 255),
           off: ChunkyPNG::Color.rgba(  0,  50,   0, 255)}

  def color light
    COLORS[light]
  end

end

l = Light.new(1000, 1000)

l.process_file 'input.txt'
puts l.count_active_lights

l.to_png 'lights.png'
