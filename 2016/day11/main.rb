
FLOORS = {first: 0, second: 1, third: 2, fourth: 3}
INPUT = [
  [[:generator, "polonium"], [:generator, "thulium"],[:microchip, "thulium"],[:generator, "promethium"],[:generator, "ruthenium"],[:microchip, "ruthenium"],[:generator, "cobalt"],[:microchip, "cobalt"]],
  [[:microchip, "polonium"], [:microchip, "promethium"]],
  [],
  [],
]

class State
  def initialize(floors, floor, movements)
    @floors = floors
    @floor = floor
    @movements = movements
  end

  def valid?
    @floors.all?(&method(:floor_valid?))
  end

  def done?
    @floors[0,3].all?(&:empty?)
  end

  def floor_valid?(floor_index)
    microchips = floor(floor_index).select { |item| item.first == :microchip }.map(&:last)
    generators = floor(floor_index).select { |item| item.first == :generator }.map(&:last)
    unshielded_generators = generators - microchips
    unshielded_microchips = microchips - generators
    fried_microchips = unshielded_microchips & unshielded_generators
    fried_microchips.empty?
  end

  def possible_moves
    new_movements = @movements + 1
    [1, -1].map do |floor_diff|
      new_floor_index = @floor + floor_diff
      if new_floor_index > 0 && new_floor_index < 4
        takes = [1,2].map { |n| current_floor.combination(n).to_a }.flatten(1)
        takes.map do |taken|
          new_floors = @floors.dup
          new_floors[@floor] = current_floor - taken
          new_floors[new_floor_index] = floor(new_floor_index) + taken
          new_state = State.new(new_floors, new_floor_index, new_movements)
          if new_state.floor_valid?(@floor) && new_state.floor_valid?(new_floor_index)
            new_state
          end
        end
      end
    end.flatten(1).compact
  end

  def current_floor
    @floors[@floor]
  end

  def floor(floor_index)
    @floors[floor_index]
  end

  def to_s
    @floors.each_with_index.map do |floor, floor_index|
      contents = floor.map do |item|
        item.last[0,2].capitalize + item.first[0].capitalize
      end
      "F#{floor_index} #{floor_index == @floor ? "E" : "."} #{contents.join(" ")}"
    end.join("\n")
  end
end

states = [State.new(INPUT, 0, 0)]
puts states.first.to_s

# One step
# state.possible_moves.each do |new_state|
#   puts "####"
#   puts new_state.to_s
# end

# BFS
# while !states.any?(&:done?)
#   puts states.size
#   states = states.map(&:possible_moves).flatten(1)
# end

# DFS
# while !states.any?(&:done?)
#   state = states.pop
#   states = states.map(&:possible_moves).flatten(1) + states
# end

class Stategy
  def do

  end
end
