defmodule Mint.WebSocket.Extension do
  @moduledoc """
  Tools for defining extensions to the WebSocket protocol

  The WebSocket protocol allows for extensions which act as middle-ware
  in the encoding and decoding of frames. In `Mint.WebSocket`, extensions are
  written as module which implement the `Mint.WebSocket.Extension` behaviour.

  The common "permessage-deflate" extension is built-in to `Mint.WebSocket` as
  `Mint.WebSocket.PerMessageDeflate`. This extension should be used as a
  reference when writing future extensions, but future extensions should be
  written as separate libraries which extend `Mint.WebSocket` instead of
  built-in. Also note that extensions must operate on the internal
  representations of frames using the records defined in `Mint.WebSocket.Frame`,
  which are not documented.
  """

  alias Mint.WebSocketError

  @typedoc """
  Parameters to configure an extension

  Some extensions can be configured by negotiation between the client and
  server. For example "permessage-deflate" usually shows up as in the
  "sec-websocket-extensions" header literally like so:

  ```text
  Sec-WebSocket-Extensions: permessage-deflate
  ```

  But the zlib window sizes and reset behavior can be negotiated with parameters
  with headers like so

  ```text
  Sec-WebSocket-Extensions: permessage-deflate; client_no_context_takeover; client_max_window_bits=12
  ```

  These can be configured by passing parameters to any element passed in the
  `:extensions` option to `Mint.WebSocket.upgrade/4`.

  For example, one might write the above parameter configuration as

  ```elixir
  [
    {Mint.WebSocket.PerMessageDeflate,
     [client_no_context_takeover: true, client_max_window_bits: 12]}
  ]
  ```

  when passing the `:extensions` option to `Mint.WebSocket.upgrade/4`.

  Note that `Mint.WebSocket.upgrade/4` will normalize the parameters of an
  extension to a list of two-tuples with string keys and values. For example,
  the above would be normalized to this extensions list:

  ```elixir
  [
    %Mint.WebSocket.Extension{
      name: "permessage-deflate",
      module: Mint.WebSocket.PerMessageDeflate,
      params: [
        {"client_no_context_takeover", "true"},
        {"client_max_window_bits", "12"}
      ],
      state: nil,
      opts: []
    }
  ]
  ```
  """
  @type params :: [atom() | String.t() | {atom() | String.t(), boolean() | String.t()}]

  @typedoc """
  A structure representing an instance of an extension

  Extensions are implemented as modules but passed to `Mint.WebSocket` as
  `Mint.WebSocket.Extension` structs with the following keys:

  * `:name` - the name of the extension. When using the short-hand tuple
    syntax to pass extensions to `Mint.WebSocket.upgrade/4`, the name is
    determined by calling the `c:name/0` callback.
  * `:module` - the module which implements the callbacks defined in the
    `Mint.WebSocket.Extension` behavior.
  * `:state` - an arbitrary piece of data curated by the extension. For
    example, the "permessage-deflate" extension uses this field to
    hold `t:zlib.zstream()`s for compression and decompression.
  * `:params` - a list with key-value tuples or atom/string keys which configure
    the parameters communicated to the server. All params are encoded into the
    "sec-websocket-extensions" header. Also see the documentation for
    `t:params/0`.
  * `:opts` - a keyword list to pass configuration to the extension. These
    are not encoded into the "sec-websocket-extensions" header. For example,
    `:opts` is used by the "permessage-deflate" extension to configure `:zlib`
    configuration.
  """
  @type t :: %__MODULE__{
          name: String.t(),
          module: module(),
          state: term(),
          params: params(),
          opts: Keyword.t()
        }

  @doc """
  Returns the name of the WebSocket extension

  This should not include the parameters for the extension, such as
  "client_max_window_bits" for the "permessage-deflate" extension.

  ## Examples

      iex> Mint.WebSocket.PerMessageDeflate.name()
      "permessage-deflate"
  """
  @callback name() :: String.t()

  @doc """
  Invoked when the WebSocket server accepts an extension

  This callback should be used to initialize any `:state` that the extension
  needs to operate. For example, this callback is used by the
  "permessage-deflate" extension to setup `t::zlib.zstream()`s and store
  them in state.

  The `all_extensions` argument is passed so that the extension can know
  about the existence and ordering of other extensions. This can be useful
  if a client declares multiple extensions which accomplish the same job
  (such as multiple compression extensions) but want to only enable one based
  on what the server accepts.

  Note that extensions are initialized in the order in which the server accepts
  them: any extensions preceeding `this_extension` in `all_extensions` are
  initialized while any extensions after `this_extension` are not yet
  initialized.

  Error tuples bubble up to `Mint.WebSocket.upgrade/4`.
  """
  @callback init(this_extension :: t(), all_extensions :: t()) :: {:ok, t()} | {:error, term()}

  @doc """
  Invoked when encoding frames before sending them across the wire

  Error tuples bubble up to `Mint.WebSocket.encode/2`.
  """
  @callback encode(frame :: tuple(), state :: term()) ::
              {:ok, frame :: tuple(), state :: term()} | {:error, term()}

  @doc """
  Invoked when decoding frames after receiving them from the wire

  Error tuples bubble up to `Mint.WebSocket.decode/2`.
  """
  @callback decode(frame :: tuple(), state :: term()) ::
              {:ok, frame :: tuple(), state :: term()} | {:error, term()}

  defstruct [:name, :module, :state, opts: [], params: []]

  @doc false
  @spec encode(tuple(), [t()]) :: {tuple(), [t()]} | no_return()
  def encode(frame, extensions) do
    encode_all_extensions(frame, extensions, [])
  end

  defp encode_all_extensions(frame, extensions, acc)

  defp encode_all_extensions(frame, [], acc), do: {frame, :lists.reverse(acc)}

  defp encode_all_extensions(frame, [extension | extensions], acc) do
    case extension.module.encode(frame, extension.state) do
      {:ok, frame, new_state} ->
        encode_all_extensions(
          frame,
          extensions,
          [put_in(extension.state, new_state) | acc]
        )

      {:error, reason} ->
        throw({:mint, reason})
    end
  end

  @doc false
  @spec decode(tuple(), [t()]) :: {tuple(), [t()]} | {{:error, term()}, [t()]}
  def decode(frame, extensions) do
    decode_all_extensions(frame, extensions, [])
  end

  defp decode_all_extensions(frame, extensions, acc)

  defp decode_all_extensions(frame, [], acc), do: {frame, :lists.reverse(acc)}

  defp decode_all_extensions(frame, [extension | extensions], acc) do
    case extension.module.decode(frame, extension.state) do
      {:ok, frame, new_state} ->
        decode_all_extensions(
          frame,
          extensions,
          [put_in(extension.state, new_state) | acc]
        )

      {:error, reason} ->
        {{:error, reason}, :lists.reverse(acc)}
    end
  end

  @doc false
  def accept_extensions(client_extensions, response_headers) do
    server_extensions = parse_accepted_extensions(response_headers)
    client_extension_mapping = Enum.into(client_extensions, %{}, &{&1.name, &1})

    accept_extensions(server_extensions, [], client_extension_mapping)
  end

  defp accept_extensions(server_extension, acc, client_extension_mapping)

  defp accept_extensions([], acc, _), do: {:ok, :lists.reverse(acc)}

  defp accept_extensions([server_extension | server_extensions], acc, client_extension_mapping) do
    with {:ok, client_extension} <-
           Map.fetch(client_extension_mapping, server_extension.name),
         extension = %__MODULE__{client_extension | params: server_extension.params},
         all_extensions = acc ++ [extension | server_extensions],
         {:ok, extension} <- client_extension.module.init(extension, all_extensions) do
      accept_extensions(
        server_extensions,
        acc ++ [extension],
        client_extension_mapping
      )
    else
      :error ->
        {:error, %WebSocketError{reason: {:extension_not_negotiated, server_extension}}}

      init_error ->
        init_error
    end
  end

  # There may be multiple sec-websocket-extension headers and these
  # should be treated as separate items, or many extensions may be declared
  # in one header.
  #
  # As [the RFC](https://datatracker.ietf.org/doc/html/rfc6455#section-9.1)
  # says:
  #
  # Note that like other HTTP header fields, this header field MAY be
  # split or combined across multiple lines.  Ergo, the following are
  # equivalent:
  #
  #       Sec-WebSocket-Extensions: foo
  #       Sec-WebSocket-Extensions: bar; baz=2
  #
  # is exactly equivalent to
  #
  #       Sec-WebSocket-Extensions: foo, bar; baz=2
  defp parse_accepted_extensions(response_headers) do
    response_headers
    |> Enum.flat_map(fn
      {"sec-websocket-extensions", extension_string} ->
        extension_string
        |> String.split(", ")
        |> Enum.map(&parse_extension/1)

      _ ->
        []
    end)
  end

  defp parse_extension(extension_string) do
    [name | params] = String.split(extension_string, ";")

    params =
      Enum.map(params, fn param ->
        param_tokens =
          param
          |> String.trim()
          |> String.split("=", parts: 2)

        case param_tokens do
          [param] -> {param, "true"}
          [param, value] -> {param, value}
        end
      end)

    %__MODULE__{name: name, params: params}
  end
end
