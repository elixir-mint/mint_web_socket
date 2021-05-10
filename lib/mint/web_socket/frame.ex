defmodule Mint.WebSocket.Frame do
  @moduledoc false

  # Functions and data structures for describing websocket frames.
  # https://tools.ietf.org/html/rfc6455#section-5.2

  shared = [{:reserved, <<0::size(3)>>}, :mask, :data]

  import Record
  alias Mint.WebSocket.{Utils, Frame.Fragment}

  defrecord :continuation, shared ++ [:fin?]
  defrecord :text, shared ++ [:fin?]
  defrecord :binary, shared ++ [:fin?]
  # > All control frames MUST have a payload length of 125 bytes or less
  # > and MUST NOT be fragmented.
  defrecord :close, shared ++ [:code, :reason]
  defrecord :ping, shared
  defrecord :pong, shared

  defguard is_control(frame)
           when is_tuple(frame) and
                  (elem(frame, 0) == :close or elem(frame, 0) == :ping or elem(frame, 0) == :pong)

  defguard is_fin(frame)
           when (elem(frame, 0) in [:continuation, :text, :binary] and elem(frame, 4) == true) or
                  is_control(frame)

  # https://tools.ietf.org/html/rfc6455#section-7.4.1
  @invalid_status_codes [1_004, 1_005, 1_006, 1_016, 1_100, 2_000, 2_999]
  # https://tools.ietf.org/html/rfc6455#section-7.4.2
  defguardp is_valid_close_code(code) when code in 1_000..4_999 and code not in @invalid_status_codes

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
  @non_control_opcodes [:continuation, :text, :binary]

  def new_mask, do: :crypto.strong_rand_bytes(4)

  @spec encode(tuple()) :: {:ok, binary()} | {:error, :payload_too_large}
  def encode(frame) do
    payload = payload(frame)
    mask = mask(frame)
    masked? = if mask == nil, do: 0, else: 1

    with {:ok, encoded_payload_length} <-
           encode_payload_length(elem(frame, 0), byte_size(payload)) do
      {:ok,
       <<
         encode_fin(frame)::bitstring,
         reserved(frame)::bitstring,
         encode_opcode(frame)::bitstring,
         masked?::size(1),
         encoded_payload_length::bitstring,
         mask || <<>>::binary,
         apply_mask(payload, mask)::bitstring
       >>}
    end
  end

  for type <- Map.keys(@opcodes) do
    defp payload(unquote(type)(data: data)), do: data
    defp mask(unquote(type)(mask: mask)), do: mask
    defp reserved(unquote(type)(reserved: reserved)), do: reserved
  end

  defp encode_fin(text(fin?: false)), do: <<0b0::size(1)>>
  defp encode_fin(binary(fin?: false)), do: <<0b0::size(1)>>
  defp encode_fin(continuation(fin?: false)), do: <<0b0::size(1)>>
  defp encode_fin(_), do: <<0b1::size(1)>>

  defp encode_opcode(frame), do: @opcodes[elem(frame, 0)]

  def encode_payload_length(_opcode, length) when length in 0..125 do
    {:ok, <<length::integer-size(7)>>}
  end

  def encode_payload_length(opcode, length)
      when length in 126..65_535 and opcode in @non_control_opcodes do
    {:ok, <<126::integer-size(7), length::unsigned-integer-size(8)-unit(2)>>}
  end

  def encode_payload_length(opcode, length)
      when length in 65_535..9_223_372_036_854_775_807 and opcode in @non_control_opcodes do
    {:ok, <<127::integer-size(7), length::unsigned-integer-size(8)-unit(8)>>}
  end

  def encode_payload_length(_opcode, _length) do
    {:error, :payload_too_large}
  end

  # Mask the payload by bytewise XOR-ing the payload bytes against the mask
  # bytes (where the mask bytes repeat).
  # This is an "involution" function: applying the mask will mask
  # the data and applying the mask again will unmask it.
  def apply_mask(payload, nil), do: payload

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

  @spec decode(Mint.WebSocket.t(), binary()) ::
          {:ok, Mint.WebSocket.t(), [Mint.WebSocket.frame()]}
          | {:error, Mint.WebSocket.t(), any()}
  def decode(websocket, data) do
    case websocket.buffer |> Utils.maybe_concat(data) |> decode_raw(websocket, []) do
      {:ok, frames} ->
        {websocket, frames} = Fragment.resolve(websocket, frames)
        {:ok, put_in(websocket.buffer, <<>>), frames}

      {:buffer, partial, frames} ->
        {websocket, frames} = Fragment.resolve(websocket, frames)
        {:ok, put_in(websocket.buffer, partial), frames}
    end
  catch
    :throw, {:mint, reason} -> {:error, websocket, reason}
  end

  defp decode_raw(
         <<fin::size(1), reserved::bitstring-size(3), opcode::bitstring-size(4), masked::size(1),
           payload_and_mask::bitstring>> = data,
         websocket,
         acc
       ) do
    case decode_payload_and_mask(payload_and_mask, masked == 0b1) do
      {:ok, payload, mask, rest} ->
        decode_raw(rest, websocket, [
          decode(
            decode_opcode(opcode),
            fin == 0b1,
            decode_reserved(reserved, websocket.extensions),
            mask,
            apply_mask(payload, mask)
          )
          | acc
        ])

      :buffer ->
        {:buffer, data, :lists.reverse(acc)}
    end
  end

  defp decode_raw(<<>>, _websocket, acc), do: {:ok, :lists.reverse(acc)}

  defp decode_raw(partial, _websocket, acc) when is_binary(partial) do
    {:buffer, partial, :lists.reverse(acc)}
  end

  defp decode_opcode(opcode) do
    case Map.fetch(@reverse_opcodes, opcode) do
      {:ok, opcode_atom} ->
        opcode_atom

      :error ->
        throw({:mint, {:unsupported_opcode, opcode}})
    end
  end

  # this will need to be updated to support extensions
  defp decode_reserved(<<0::size(3)>> = reserved, _extensions) do
    reserved
  end

  defp decode_reserved(malformed_reserved, _extensions) do
    throw({:mint, {:malformed_reserved, malformed_reserved}})
  end

  defp decode_payload_and_mask(payload, masked?) do
    {payload_length, rest} = decode_payload_length(payload)
    {mask, rest} = decode_mask(rest, masked?)

    case rest do
      <<payload::binary-size(payload_length), more::bitstring>> ->
        {:ok, payload, mask, more}

      partial when is_binary(partial) ->
        :buffer
    end
  end

  defp decode_payload_length(
         <<127::integer-size(7), payload_length::unsigned-integer-size(8)-unit(8),
           rest::bitstring>>
       ),
       do: {payload_length, rest}

  defp decode_payload_length(
         <<126::integer-size(7), payload_length::unsigned-integer-size(8)-unit(2),
           rest::bitstring>>
       ),
       do: {payload_length, rest}

  defp decode_payload_length(<<payload_length::integer-size(7), rest::bitstring>>)
       when payload_length in 0..125,
       do: {payload_length, rest}

  defp decode_payload_length(malformed) do
    throw({:mint, {:malformed_payload_length, malformed}})
  end

  defp decode_mask(<<mask::binary-size(8)-unit(4), rest::bitstring>>, _masked? = true) do
    {mask, rest}
  end

  defp decode_mask(payload, _masked? = false) do
    {nil, payload}
  end

  defp decode_mask(payload, _masked?) do
    throw({:mint, {:missing_mask, payload}})
  end

  for data_type <- [:continuation, :text, :binary] do
    def decode(unquote(data_type), fin?, reserved, mask, payload) do
      unquote(data_type)(
        fin?: fin?,
        reserved: reserved,
        mask: mask,
        data: payload
      )
    end
  end

  def decode(
        :close,
        _fin?,
        reserved,
        mask,
        <<code::unsigned-integer-size(8)-unit(2), reason::binary>> = payload
      ) when byte_size(reason) in 0..123 and is_valid_close_code(code) do
    if String.valid?(reason) do
      close(reserved: reserved, mask: mask, code: code, reason: reason)
    else
      throw({:mint, {:invalid_close_payload, payload}})
    end
  end

  def decode(
        :close,
        _fin?,
        reserved,
        mask,
        <<>>
      ) do
    close(reserved: reserved, mask: mask, code: 1_000, reason: "")
  end

  def decode(
        :close,
        _fin?,
        _reserved,
        _mask,
        payload
      ) do
    throw({:mint, {:invalid_close_payload, payload}})
  end

  def decode(:ping, _fin?, reserved, mask, payload) do
    ping(reserved: reserved, mask: mask, data: payload)
  end

  def decode(:pong, _fin?, reserved, mask, payload) do
    pong(reserved: reserved, mask: mask, data: payload)
  end

  # translate from user-friendly tuple into record defined in this module
  # (and the reverse)
  @spec translate(Mint.WebSocket.frame()) :: tuple()
  @spec translate(tuple) :: Mint.WebSocket.frame()
  def translate({:text, text}) do
    text(fin?: true, mask: new_mask(), data: text)
  end

  def translate(text(fin?: true, data: data)) do
    if String.valid?(data) do
      {:text, data}
    else
      throw({:mint, {:invalid_utf8, data}})
    end
  end

  def translate({:binary, binary}) do
    binary(fin?: true, mask: new_mask(), data: binary)
  end

  def translate(binary(fin?: true, data: data)), do: {:binary, data}

  def translate(:ping), do: translate({:ping, <<>>})

  def translate({:ping, body}) do
    ping(mask: new_mask(), data: body)
  end

  def translate(ping(data: <<>>)), do: :ping

  def translate(ping(data: data)), do: {:ping, data}

  def translate(:pong), do: translate({:pong, <<>>})

  def translate({:pong, body}) do
    pong(mask: new_mask(), data: body)
  end

  def translate(pong(data: <<>>)), do: :pong

  def translate(pong(data: data)), do: {:pong, data}

  def translate(:close) do
    close(mask: new_mask(), data: <<>>)
  end

  def translate({:close, code, reason})
      when is_integer(code) and is_binary(reason) do
    close(mask: new_mask(), data: encode_close(code, reason))
  end

  def translate(close(code: nil, reason: nil)), do: :close

  def translate(close(code: code, reason: reason)) do
    {:close, code, reason}
  end

  defp encode_close(code, reason) do
    <<code::unsigned-integer-size(8)-unit(2), reason::binary>>
  end

  for type <- [:continuation, :text, :binary] do
    def combine(
          unquote(type)(data: frame_data) = frame,
          continuation(data: continuation_data, fin?: fin?)
        ) do
      unquote(type)(frame, data: frame_data <> continuation_data, fin?: fin?)
    end
  end
end
