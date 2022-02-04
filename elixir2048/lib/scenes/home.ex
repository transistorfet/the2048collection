defmodule Elixir2048.Scene.Home do
  use Scenic.Scene
  require Logger

  alias Scenic.Graph
  alias Scenic.ViewPort

  import Scenic.Primitives
  # import Scenic.Components

  @text_size 24

  def generate_world() do
    [
      [ 0, 2, 0, 0 ],
      [ 4, 0, 0, 2 ],
      [ 0, 0, 0, 2 ],
      [ 0, 8, 0, 0 ]
    ]
  end

  def collapse_row(row) do
    case row do
      [] -> []
      [0 | remain] -> collapse_row(remain)
      [x, 0 | remain] -> collapse_row([x] ++ remain)
      [x, y | remain] when x == y -> [(x + y)] ++ collapse_row(remain)
      [x | remain] -> [x] ++ collapse_row(remain)
    end
  end

  def fill_zeros(len) do
    case len do
      0 -> []
      1 -> if Enum.random(1..100) < 10 do [2] else [0] end
      _ -> [0] ++ fill_zeros(len - 1)
    end
  end

  def to_columns(rows) do
    Enum.map(0..3, fn (i) ->
      Enum.map(rows, fn (row) ->
        Enum.at(row, i)
      end)
    end)
  end

  def shift_world_up(world) do
    to_columns(Enum.map(to_columns(world), fn (row) ->
      new_row = collapse_row(row)
      new_row ++ fill_zeros(4 - length(new_row))
    end))
  end

  def shift_world_down(world) do
    to_columns(Enum.map(to_columns(world), fn (row) ->
      new_row = collapse_row(row)
      Enum.reverse(fill_zeros(4 - length(new_row))) ++ new_row
    end))
  end

  def shift_world_left(world) do
    Enum.map(world, fn (row) ->
      new_row = collapse_row(row)
      new_row ++ fill_zeros(4 - length(new_row))
    end)
  end

  def shift_world_right(world) do
    Enum.map(world, fn (row) ->
      new_row = collapse_row(row)
      Enum.reverse(fill_zeros(4 - length(new_row))) ++ new_row
    end)
  end

  def draw_square(x, y, num) do
    x_pos = 100 + (x * 100)
    y_pos = 100 + (y * 100)

    [ text_spec(to_string(num), translate: {x_pos + 40, y_pos + 40}),
      rect_spec({80, 80}, stroke: {2, :gray}, translate: {x_pos, y_pos})]
  end

  def draw_world(rows) do
    Enum.reduce(Enum.with_index(rows), [], fn ({row, y}, acc) ->
      Enum.reduce(Enum.with_index(row), acc, fn ({num, x}, acc) ->
        acc ++ draw_square(x, y, num)
      end)
    end)
  end

  def update_graph(world) do
    Graph.build(font: :roboto, font_size: @text_size)
    |> add_specs_to_graph(draw_world(world))
  end

  def init(_, opts) do
    state = %{
      world: generate_world(),
    }

    graph = update_graph(state.world)

    {:ok, state, push: graph}
  end

  def handle_input(event, _context, state) do
    #Logger.info("Received event: #{inspect(event)}")

    new_world = case event do
      {:key, {"up", :press, 0}} -> shift_world_up(state.world)
      {:key, {"down", :press, 0}} -> shift_world_down(state.world)
      {:key, {"left", :press, 0}} -> shift_world_left(state.world)
      {:key, {"right", :press, 0}} -> shift_world_right(state.world)
      _ -> state.world
    end

    state = %{
      world: new_world
    }

    graph = update_graph(state.world)
    {:noreply, state, push: graph }
  end
end
