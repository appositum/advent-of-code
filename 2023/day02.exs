defmodule Main do
  def main() do
    input = String.trim File.read!("inputs/day02.txt")

    games = String.split(input, "\n")
    |> Enum.map(fn game ->
      Regex.replace(~r/Game \w+: /, game, "")
      |> String.split("; ")
      |> Enum.map(fn round ->
        String.split(round, ", ")
        |> Enum.map(fn cubes ->
          [quantity, color] = String.split(cubes, " ")
          {String.to_atom(color), String.to_integer(quantity)}
        end)
        |> Map.new()
        |> Map.put_new(:red, 0)
        |> Map.put_new(:green, 0)
        |> Map.put_new(:blue, 0)
      end)
    end)

    sum_ids = Enum.with_index(games, 1)
    |> Enum.filter(& valid_game?(elem(&1, 0), %{red: 12, green: 13, blue: 14}))
    |> Enum.reduce(0, fn x, acc ->
      elem(x, 1) + acc
    end)

    sum_powers = Enum.sum Enum.map games, fn x ->
      power_sum(x)
    end

    IO.puts "part 1: #{sum_ids}"
    IO.puts "part 2: #{sum_powers}"
  end

  @type max_cubes() :: %{key: integer()}
  @type round() :: %{key: integer()}
  @type game() :: list(round())

  @spec valid_round?(round(), max_cubes()) :: boolean()
  def valid_round?(round, max_cubes) do
    max_cubes.red >= round.red
    && max_cubes.green >= round.green
    && max_cubes.blue >= round.blue
  end

  @spec valid_game?(game(), max_cubes()) :: boolean()
  def valid_game?(game, max_cubes) do
    Enum.all?(game, & valid_round?(&1, max_cubes))
  end

  @spec power_sum(game()) :: integer()
  def power_sum(game) do
    Enum.product [
      Enum.max_by(game, & &1.red).red,
      Enum.max_by(game, & &1.green).green,
      Enum.max_by(game, & &1.blue).blue,
    ]
  end
end

Main.main
