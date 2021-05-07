defmodule Mint.WebSocket.Frame do
  @moduledoc false

  # functions and data structures for describing websocket frames

  shared_extensions = [:extensions]
  shared_data = [:mask, :data]

  import Record

  defrecord :continuation, shared_extensions ++ shared_data ++ [:fin?]
  defrecord :text, shared_extensions ++ shared_data ++ [:fin?]
  defrecord :binary, shared_extensions ++ shared_data ++ [:fin?]
  # > All control frames MUST have a payload length of 125 bytes or less
  # > and MUST NOT be fragmented.
  defrecord :close, shared_extensions ++ shared_data ++ [:code, :reason]
  defrecord :ping, shared_extensions ++ shared_data
  defrecord :pong, shared_extensions ++ shared_data

  @opcodes %{
    # non-control opcodes:
    continuation: <<0x0::size(4)>>,
    text: <<0x1::size(4)>>,
    binary: <<0x2::size(4)>>,
    # 0x3-7 reserved for future non-control frames
    # control opcodes:
    close: <<0x8::size(4)>>,
    ping: <<0x9::size(4)>>,
    pong: <<0xA::size(4)>>
    # 0xB-F reserved for future control frames
  }
  @reverse_opcodes Map.new(@opcodes, fn {k, v} -> {v, k} end)

  def new_mask, do: :crypto.strong_rand_bytes(4)

  # https://tools.ietf.org/html/rfc6455#section-5.2
  @spec encode(tuple(), binary()) :: {:ok, iodata()} | {:error, :payload_too_large}
  def encode(frame, mask \\ new_mask()) do
    payload = payload(frame)

    with {:ok, encoded_payload_length} <-
           payload |> IO.iodata_length() |> encode_payload_length() do
      [
        encode_fin(frame),
        encode_extensions(frame),
        encode_opcode(frame),
        _masked? = <<0b1::size(1)>>,
        encoded_payload_length,
        mask,
        apply_mask(payload, mask)
      ]
    end
  end

  for type <- Map.keys(@opcodes) do
    defp payload(unquote(type)(data: data)), do: data
  end

  defp encode_fin(text(fin?: false)), do: <<0b0::size(1)>>
  defp encode_fin(binary(fin?: false)), do: <<0b0::size(1)>>
  defp encode_fin(continuation(fin?: false)), do: <<0b0::size(1)>>
  defp encode_fin(_), do: <<0b1::size(1)>>

  defp encode_extensions(_), do: <<0b000::size(3)>>

  defp encode_opcode(frame), do: @opcodes[elem(frame, 0)]

  def encode_payload_length(length) when length in 0..125 do
    {:ok, <<length::integer-size(7)>>}
  end

  def encode_payload_length(length) when length in 126..65_535 do
    {:ok, <<126::integer-size(7), length::unsigned-integer-size(8)-unit(2)>>}
  end

  def encode_payload_length(length) when length in 65_535..9_223_372_036_854_775_807 do
    {:ok, <<127::integer-size(7), length::unsigned-integer-size(8)-unit(8)>>}
  end

  def encode_payload_length(_length) do
    {:error, :payload_too_large}
  end

  # Mask the payload by bytewise XOR-ing the payload bytes against the mask
  # bytes (where the mask bytes repeat).
  # This is an "involution" function: applying the mask will mask
  # the data and applying the mask again will unmask it.
  #
  # YARD refactor to work on iodata?
  def apply_mask(payload, _mask = <<a, b, c, d>>) do
    [a, b, c, d]
    |> Stream.cycle()
    |> Enum.reduce_while({payload, _acc = <<>>}, fn
      _mask_key, {<<>>, acc} ->
        {:halt, acc}

      mask_key, {<<part_key::integer, payload_rest::binary>>, acc} ->
        {:cont, {payload_rest, <<acc::binary, Bitwise.bxor(mask_key, part_key)::integer>>}}
    end)
  end

  @spec decode(binary()) :: {:ok, tuple()} | any()
  def decode(
        <<fin::size(1), extensions::bitstring-size(3), opcode::bitstring-size(4), masked::size(1),
          payload_and_mask::bitstring>>
      ) do
    {payload, mask} = decode_payload_and_mask(payload_and_mask, masked == 0b1)

    decode(
      @reverse_opcodes[opcode],
      fin == 0b1,
      extensions,
      mask,
      payload
    )
  end

  defp decode_payload_and_mask(
         <<127::integer-size(7), payload_length::unsigned-integer-size(8)-unit(8),
           mask::binary-size(8)-unit(4), masked_payload::binary-size(payload_length)>>,
         _masked? = true
       ) do
    {apply_mask(masked_payload, mask), mask}
  end

  defp decode_payload_and_mask(
         <<126::integer-size(7), payload_length::unsigned-integer-size(8)-unit(2),
           mask::binary-size(8)-unit(4), masked_payload::binary-size(payload_length)>>,
         _masked? = true
       ) do
    {apply_mask(masked_payload, mask), mask}
  end

  defp decode_payload_and_mask(
         <<payload_length::integer-size(7), mask::binary-size(8)-unit(4),
           masked_payload::binary-size(payload_length)>>,
         _masked? = true
       )
       when payload_length in 0..125 do
    {apply_mask(masked_payload, mask), mask}
  end

  defp decode_payload_and_mask(
         <<127::integer-size(7), payload_length::unsigned-integer-size(8)-unit(8),
           payload::binary-size(payload_length)>>,
         _masked? = false
       ) do
    {payload, nil}
  end

  defp decode_payload_and_mask(
         <<126::integer-size(7), payload_length::unsigned-integer-size(8)-unit(2),
           payload::binary-size(payload_length)>>,
         _masked? = false
       ) do
    {payload, nil}
  end

  defp decode_payload_and_mask(
         <<payload_length::integer-size(7), payload::binary-size(payload_length)>>,
         _masked? = false
       )
       when payload_length in 0..125 do
    {payload, nil}
  end

  def rops, do: @reverse_opcodes

  for data_type <- [:continuation, :text, :binary] do
    def decode(unquote(data_type), fin?, extensions, mask, payload) do
      unquote(data_type)(
        fin?: fin?,
        extensions: extensions,
        mask: mask,
        data: payload
      )
    end
  end

  def decode(:close, _fin?, extensions, mask, payload) do
    close(extensions: extensions, mask: mask, data: payload)
  end

  def decode(:ping, _fin?, extensions, mask, payload) do
    ping(extensions: extensions, mask: mask, data: payload)
  end

  def decode(:pong, _fin?, extensions, mask, payload) do
    pong(extensions: extensions, mask: mask, data: payload)
  end
end
