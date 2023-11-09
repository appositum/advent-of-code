defmodule Main do
  def main() do
    input = File.read!("inputs/day03.txt")
    |> String.trim()
    |> String.split("\n")
    |> parse()

    IO.puts "part 1: #{overlap_count(input)}"
    IO.puts "part 2: #{no_overlap(input)}"
  end

  def parse(lines) do
    Enum.map lines, fn line ->
      [id, l, t, w, h] = Regex.split(~r/[#@,:x]/, line)
      |> Enum.filter(& &1 != "")
      |> Enum.map(& String.to_integer String.trim(&1))

      %{claim_id: id, left_edge: l, top_edge: t, width: w, height: h}
    end
  end

  def claimed_areas(claims) do
    Enum.map(claims, fn claim ->
      for x <- claim.left_edge..(claim.left_edge + claim.width - 1),
          y <- claim.top_edge..(claim.top_edge + claim.height - 1),
      do: {x, y}
    end)
    |> Enum.concat()
    |> Enum.frequencies()
  end

  def overlap_count(claims) do
    claimed_areas(claims)
    |> Map.values()
    |> Enum.count(& &1 > 1)
  end

  def no_overlap(claims) do
    no_overlap_rec(claims, claimed_areas(claims))
  end

  def no_overlap_rec([]), do: :error
  def no_overlap_rec([claim | cs], claim_count) do
    intersection = :maps.intersect claimed_areas([claim]), claim_count

    if Enum.all? Map.values(intersection), &(&1 == 1) do
      claim.claim_id
    else
      no_overlap_rec(cs, claim_count)
    end
  end
end

Main.main
