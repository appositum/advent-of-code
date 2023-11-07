defmodule Pointer do
  @type direction :: :north | :east | :south | :west

  defstruct [direction: :north, location: {0,0}]

  def turn_right(pointer, step) do
    {x, y} = pointer.location

    case pointer.direction do
      :north -> %Pointer{direction: :east, location: {x + step, y}}
      :east -> %Pointer{direction: :south, location: {x, y - step}}
      :south -> %Pointer{direction: :west, location: {x - step, y}}
      :west -> %Pointer{direction: :north, location: {x, y + step}}
    end
  end

  def turn_left(pointer, step) do
    {x, y} = pointer.location

    case pointer.direction do
      :north -> %Pointer{direction: :west, location: {x - step, y}}
      :east -> %Pointer{direction: :north, location: {x, y + step}}
      :south -> %Pointer{direction: :east, location: {x + step, y}}
      :west -> %Pointer{direction: :south, location: {x, y - step}}
    end
  end

  def parse_instruction(str, initial_pointer) do
    {direction, step} = String.split_at(str, 1)
    step = String.to_integer(step)

    case direction do
      "R" -> Pointer.turn_right(initial_pointer, step)
      "L" -> Pointer.turn_left(initial_pointer, step)
    end
  end
end

defmodule Main do
  def main do
    input = File.read!("inputs/day01.txt")
    |> String.trim()
    |> String.split(", ")

    destination = List.foldl(input, %Pointer{}, &Pointer.parse_instruction/2)

    IO.puts "part 1: #{distance(destination.location)}"

    {:some, visited_twice} = [%Pointer{} | Enum.scan(input, %Pointer{}, &Pointer.parse_instruction/2)]
    |> location_visited_twice()

    IO.puts "part 2: #{distance(visited_twice)}"
  end

  def distance({x, y}) do
    abs(x) + abs(y)
  end

  def range({x1, y1}, {x2, y2}) when x1 == x2 do
    Enum.map(y1..y2, & {x2, &1})
  end

  def range({x1, y1}, {x2, y2}) when y1 == y2 do
    Enum.map(x1..x2, & {&1, y2})
  end

  def locations_visited([]), do: []
  def locations_visited([_pointer | []]), do: []
  def locations_visited([pointer1 | [pointer2 | pointers]]) do
    locations = range(pointer1.location, pointer2.location)

    case locations do
      [] -> locations_visited([pointer2 | pointers])
      [_head | tail] -> tail ++ locations_visited([pointer2 | pointers])
    end
  end

  def rec([], _map_set), do: :none
  def rec([l | locations], map_set) do
    if MapSet.member?(map_set, l) do
      {:some, l}
    else
      rec(locations, MapSet.put(map_set, l))
    end
  end

  def location_visited_twice(pointers) do
    locations_visited(pointers)
    |> rec(MapSet.new())
  end
end

Main.main()
