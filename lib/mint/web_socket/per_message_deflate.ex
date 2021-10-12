defmodule Mint.WebSocket.PerMessageDeflate do
  @moduledoc """
  A WebSocket extension which compresses each message before sending it across
  the wire

  This extension is defined in
  [rfc7692](https://www.rfc-editor.org/rfc/rfc7692.html).

  ## Options

  * `:zlib_level` - (default: `:best_compression`) the compression level to
    use for the deflation zstream. See the `:zlib.deflateInit/6` documentation
    on the `Level` argument.
  * `:zlib_memory_level` - (default: `8`) how much memory to allow for use
    during compression. See the `:zlib.deflateInit/6` documentation on the
    `MemLevel` argument.
  """

  require Mint.WebSocket.Frame, as: Frame
  alias Mint.WebSocket.Extension

  @typedoc false
  @type t :: %__MODULE__{
          inflate: :zlib.zstream(),
          deflate: :zlib.zstream(),
          inflate_takeover?: boolean(),
          deflate_takeover?: boolean()
        }

  defstruct [:inflate, :deflate, :inflate_takeover?, :deflate_takeover?]

  @behaviour Extension

  @doc false
  @impl Extension
  def name, do: "permessage-deflate"

  @doc false
  @impl Extension
  def init(%Extension{params: params, opts: opts} = this_extension, _other_extensions) do
    inflate_window_bits = get_window_bits(params, "server_max_window_bits", 15)
    deflate_window_bits = get_window_bits(params, "client_max_window_bits", 15)
    inflate_zstream = :zlib.open()
    deflate_zstream = :zlib.open()

    :ok = :zlib.inflateInit(inflate_zstream, -inflate_window_bits)

    :ok =
      :zlib.deflateInit(
        deflate_zstream,
        Keyword.get(opts, :zlib_level, :best_compression),
        :deflated,
        -deflate_window_bits,
        Keyword.get(opts, :zlib_memory_level, 8),
        :default
      )

    state = %__MODULE__{
      inflate: inflate_zstream,
      deflate: deflate_zstream,
      inflate_takeover?: get_takeover(params, "server_no_context_takeover", true),
      deflate_takeover?: get_takeover(params, "client_no_context_takeover", true)
    }

    {:ok, put_in(this_extension.state, state)}
  end

  @doc false
  @impl Extension
  def decode(frame, state)

  # rfc section 6: "[Per-Message Compression Extensions]s operate only on data
  # messages"
  for opcode <- [:text, :binary, :continuation] do
    def decode(
          Frame.unquote(opcode)(
            reserved: <<1::size(1), _::bitstring>> = reserved_binary,
            data: data
          ) = frame,
          state
        ) do
      <<reserved::size(3)>> = reserved_binary

      # Append 4 octets of 0x00 0x00 0xff 0xff to the tail end of the
      # payload of the message

      data =
        state.inflate
        |> :zlib.inflate(<<data::binary, 0x00, 0x00, 0xFF, 0xFF>>)
        |> IO.iodata_to_binary()

      if state.inflate_takeover? == false do
        :zlib.inflateReset(state.inflate)
      end

      frame =
        Frame.unquote(opcode)(frame,
          reserved: <<:erlang.bxor(reserved, 0b100)::size(3)>>,
          data: data
        )

      {:ok, frame, state}
    end
  end

  def decode(frame, state), do: {:ok, frame, state}

  @doc false
  @impl Extension
  def encode(frame, state)

  for opcode <- [:text, :binary, :continuation] do
    def encode(
          Frame.unquote(opcode)(
            reserved: <<0::size(1), _::bitstring>> = reserved_binary,
            data: data
          ) = frame,
          state
        ) do
      <<reserved::size(3)>> = reserved_binary

      data = deflate_data(state.deflate, data)

      if state.deflate_takeover? == false do
        :zlib.deflateReset(state.deflate)
      end

      frame =
        Frame.unquote(opcode)(frame,
          reserved: <<:erlang.bor(reserved, 0b100)::size(3)>>,
          data: data
        )

      {:ok, frame, state}
    end
  end

  def encode(frame, state), do: {:ok, frame, state}

  defp deflate_data(deflate_zstream, data) do
    deflated =
      deflate_zstream
      |> :zlib.deflate(data, :sync)
      |> IO.iodata_to_binary()

    # "Remove 4 octets (that are 0x00 0x00 0xff 0xff) from the tail end"
    data_size = byte_size(deflated) - 4

    case deflated do
      <<deflated::binary-size(data_size), 0x00, 0x00, 0xFF, 0xFF>> -> deflated
      deflated -> deflated
    end
  end

  defp get_window_bits(params, param_name, default) do
    with {:ok, value} <- fetch_param(params, param_name),
         {bits, _} <- Integer.parse(value) do
      bits
    else
      _ -> default
    end
  end

  defp get_takeover(params, param_name, default) when is_boolean(default) do
    with {:ok, value} <- fetch_param(params, param_name),
         {:ok, no_takeover?} <- parse_boolean(value) do
      not no_takeover?
    else
      _ -> default
    end
  end

  defp fetch_param(params, param_name) do
    with {^param_name, value} <- List.keyfind(params, param_name, 0, :error) do
      {:ok, value}
    end
  end

  defp parse_boolean("true"), do: {:ok, true}
  defp parse_boolean("false"), do: {:ok, false}
  defp parse_boolean(_), do: :error
end
