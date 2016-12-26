require_relative "../day11/field_support"

class Field
  include FieldSupport

  def initialize(array)
    @array = array
  end

  def get(coords)
    x, y = coords
    @array[y][x]
  end

  def width
    @array.first.size
  end

  def height
    @array.size
  end

  def each_node
    return enum_for(:each_node) unless block_given?

    @array.each_with_index.map do |row, y|
      row.each_with_index.map do |node, x|
        yield node, [x, y]
      end
    end
  end

  protected

  attr_reader :array
end
