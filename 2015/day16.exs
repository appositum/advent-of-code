defmodule Sue do
  defstruct [
    :id,
    :children,
    :cats,
    :samoyeds,
    :pomeranians,
    :akitas,
    :vizslas,
    :goldfish,
    :trees,
    :cars,
    :perfumes,
  ]

  def eq_or_nil(nil, _), do: true
  def eq_or_nil(a, b), do: a == b

  def greater_or_nil(nil, _), do: true
  def greater_or_nil(a, b), do: a > b

  def fewer_or_nil(nil, _), do: true
  def fewer_or_nil(a, b), do: a < b

  def find_aunt_pt1(sues) do
    Enum.filter(sues, fn sue ->
      Sue.eq_or_nil(sue.children, 3) &&
      Sue.eq_or_nil(sue.cats, 7) &&
      Sue.eq_or_nil(sue.samoyeds, 2) &&
      Sue.eq_or_nil(sue.pomeranians, 3) &&
      Sue.eq_or_nil(sue.akitas, 0) &&
      Sue.eq_or_nil(sue.vizslas, 0) &&
      Sue.eq_or_nil(sue.goldfish, 5) &&
      Sue.eq_or_nil(sue.trees, 3) &&
      Sue.eq_or_nil(sue.cars, 2) &&
      Sue.eq_or_nil(sue.perfumes, 1)
    end)
    |> List.first()
  end

  def find_aunt_pt2(sues) do
    Enum.filter(sues, fn sue ->
      Sue.eq_or_nil(sue.children, 3) &&
      Sue.greater_or_nil(sue.cats, 7) &&
      Sue.eq_or_nil(sue.samoyeds, 2) &&
      Sue.fewer_or_nil(sue.pomeranians, 3) &&
      Sue.eq_or_nil(sue.akitas, 0) &&
      Sue.eq_or_nil(sue.vizslas, 0) &&
      Sue.fewer_or_nil(sue.goldfish, 5) &&
      Sue.greater_or_nil(sue.trees, 3) &&
      Sue.eq_or_nil(sue.cars, 2) &&
      Sue.eq_or_nil(sue.perfumes, 1)
    end)
    |> List.first()
  end
end

defmodule Main do
  def main() do
    aunt = File.read!("inputs/day16.txt")
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn line ->
      line
      |> String.replace(":", "")
      |> String.replace(",", "")
      |> String.split()
    end)
    |> Enum.map(fn line ->
      [_, id, prop1, prop1_num, prop2, prop2_num, prop3, prop3_num] = line
      sue = %Sue{id: String.to_integer(id)}
      sue = Map.put(sue, String.to_atom(prop1), String.to_integer(prop1_num))
      sue = Map.put(sue, String.to_atom(prop2), String.to_integer(prop2_num))
      Map.put(sue, String.to_atom(prop3), String.to_integer(prop3_num))
    end)

    IO.puts "part 1: #{(Sue.find_aunt_pt1 aunt).id}"
    IO.puts "part 2: #{(Sue.find_aunt_pt2 aunt).id}"
  end
end

Main.main()
