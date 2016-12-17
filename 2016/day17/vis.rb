require 'timeout'

require_relative "./main"

class VisualField < Field

  def visualize_step(title, current_path, current_room, open_doors)
    puts "\e[H\e[2J"
    puts "Advent Of Code, Day 17"
    puts
    puts title
    puts
    puts "#" * 9
    puts (4.times.map do |y|
      s = ""
      s += "#"
      4.times do |x|
        if current_room == [x, y]
          s += "S"
        else
          s += " "
        end
        if x != 3
          if open_doors.include?([:h, x, y])
            s += " "
          else
            s += "|"
          end
        end
      end
      if y != 3
        s += "#\n#"
        s += 4.times.map do |x|
          if open_doors.include?([:v, x, y])
            " "
            else
            "-"
          end
        end.join("#")
        s += "#"
      end
      s
    end).join("\n")
    puts "#" * 7 + " V"
    puts
    puts "Steps: #{current_path.length}"
    puts "Path Taken: #{current_path}"
  end

  def transform_open_doors(open_doors, relative_to_room)
    x, y = relative_to_room
    open_doors.map do |d|
      case d
      when "U"
        [:v, x, y - 1]
      when "D"
        [:v, x, y]
      when "L"
        [:h, x - 1, y]
      when "R"
        [:h, x, y]
      end
    end
  end

  def visualize(title, fps, path)
    current_room = [0,0]
    current_path = ""
    path.chars.each do |direction|
      open_doors = transform_open_doors(open_doors_at(current_path), current_room)
      visualize_step(title, current_path, current_room, open_doors)
      sleep(1.0/fps)
      current_path += direction
      current_room = add_coordinates(current_room, DIRECTIONS[direction])
      visualize_step(title, current_path, current_room, open_doors)
      sleep(1.0/fps)
    end
    puts
    puts "Yey! :)"
    sleep 3
  end
end

def wait_and_print(pid, messages)
  messages.each do |wait_time, message|
    begin
      puts message
      Timeout.timeout(wait_time) do
        Process.wait(pid)
        return
      end
    rescue Timeout::Error
      puts
    end
  end
  Process.wait(pid)
end

input = "mmsxrhfx"

field = VisualField.new(4, 4, input)

parent_read, child_write = IO.pipe
pid = fork do
  parent_read.close
  result = field.longest_path_to_target
  Marshal.dump(result, child_write)
  exit!(0)
end
child_write.close

field.visualize("Part One", 1.2, field.shortest_path_to_target)

puts "\e[H\e[2J"
puts
wait_and_print(pid, [
  [2, "Now looking for a longest path for part two..."],
  [2, "(actually I've been looking all the time)"],
  [2, "(...in a background process while playing the first part)"],
  [3, "(but that's no excuse for my slow code)"],
  [2, "(skalski would not approve)"],
  [9, "(just a second...)"],
  [0, "(your're machine is really slow...)"],
])
result = parent_read.read
result_path = Marshal.load(result)

field.visualize("Part Two", 5, result_path)
