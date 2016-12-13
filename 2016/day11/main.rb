
require_relative "a_star"

FLOORS = {first: 0, second: 1, third: 2, fourth: 3}
INPUT = [
  [[:generator, "polonium"], [:generator, "thulium"],[:microchip, "thulium"],[:generator, "promethium"],[:generator, "ruthenium"],[:microchip, "ruthenium"],[:generator, "cobalt"],[:microchip, "cobalt"]],
  [[:microchip, "polonium"], [:microchip, "promethium"]],
  [],
  [],
]

class StateGraph
  def neighbours_with_distance(node)
  end
end

class State
  def initialize(floors, elevator)
    @floors = floors
    @elevator = elevator
  end

  def valid?(floor_index, floors)
    floors.all?(&method(:floor_valid?))
  end

  def done?(floors)
    floors[0...-1].all?(&:empty?)
  end

  def floor_valid?(floor_index, floors)
    microchips = floor(floor_index).select { |item| item.first == :microchip }.map(&:last)
    generators = floor(floor_index).select { |item| item.first == :generator }.map(&:last)
    unshielded_microchips = microchips - generators
    unshielded_microchips.empty? || generators.empty?
  end

  def possible_moves(elevator, floors)
    [1, -1].map do |floor_diff|
      elevator + floor_diff
    end.filter do |floor_index|
      floor_index >= 0 && new_floor_index < floors.size
    end.map |next_index|
      takes = [1,2].map { |n| floors[elevator].combination(n).to_a }.flatten(1)
      takes.map do |taken|
        next_floors = floors.dup
        next_floors[elevator] -= taken
        next_floors[next_index] += taken
        next unless floor_valid?(elevator, next_floors) && floor_valid?(next_index, next_floors)
        [next_index, next_floors]
      end
    end.flatten(1)
  end

  def floors_to_s(elevator, floors)
    floors.each_with_index.map do |floor, floor_index|
      item_strings = floor.map do |item|
        item.last[0,2].capitalize + item.first[0].capitalize
      end
      "F#{floor_index} #{floor_index == elevator ? "E" : "."} #{item_strings.join(" ")}"
    end.join("\n")
  end
end

states = [State.new(INPUT, 0, 0)]
puts states.first.to_s
