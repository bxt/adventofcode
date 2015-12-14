class Race < Struct.new(:reindeer)
  def step
    reindeer.each &:step
    furthest.points += 1
  end

  def furthest
    reindeer.max_by &:distance
  end

  def leader
    reindeer.max_by &:points
  end
end

class Reindeer < Struct.new(:name, :speed, :flyDuration, :restDuration, :activity, :distance, :points)
  def initialize(name, speed, flyDuration, restDuration)
    super(name, speed, flyDuration, restDuration, initial_activity, 0, 0)
  end

  def step
    activity.step
  end

  def initial_activity
    Flying.new(self)
  end
end

class Activity < Struct.new(:reindeer, :timeIn)
  def initialize(reindeer)
    super(reindeer, 0)
  end

  def step
    self.timeIn += 1
    reindeer.distance += speed
    if timeIn >= duration
      reindeer.activity = afterwards
    end
  end
end

class Flying < Activity
  def afterwards
    Resting.new(reindeer)
  end

  def duration
    reindeer.flyDuration
  end

  def speed
    reindeer.speed
  end
end

class Resting < Activity
  def afterwards
    Flying.new(reindeer)
  end

  def duration
    reindeer.restDuration
  end

  def speed
    0
  end
end

reindeer = IO.readlines("input.txt").map do |line|
  /(?<name>\w+)[^\d]*(?<speed>\d+)[^\d]*(?<flyD>\d+)[^\d]*(?<restD>\d+)/ =~ line
  Reindeer.new(name, speed.to_i, flyD.to_i, restD.to_i)
end

race = Race.new(reindeer)

2503.times { race.step }

puts "Part 1: #{race.furthest.distance}"
puts "Part 2: #{race.leader.points}"
