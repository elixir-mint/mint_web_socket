%% Copyright (c) 2015-2020, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(gun_autobahn).
-compile(export_all).
-compile(nowarn_export_all).

autobahn_fuzzingserver() ->
	N = get_case_count(),
	run_cases(0, N),
	terminate().

get_case_count() ->
	{Pid, MRef, StreamRef} = connect("/getCaseCount"),
	receive
		{gun_ws, Pid, StreamRef, {text, N}} ->
			close(Pid, MRef),
			binary_to_integer(N);
		_Msg ->
			terminate(),
			error(failed)
	end.

run_cases(Total, Total) ->
	ok;
run_cases(N, Total) ->
	{Pid, MRef, StreamRef} = connect(["/runCase?case=", integer_to_binary(N + 1), "&agent=Gun"]),
	loop(Pid, MRef, StreamRef),
	update_reports(),
	run_cases(N + 1, Total).

loop(Pid, MRef, StreamRef) ->
	receive
		{gun_ws, Pid, StreamRef, close} ->
			gun:ws_send(Pid, StreamRef, close),
			loop(Pid, MRef, StreamRef);
		{gun_ws, Pid, StreamRef, {close, Code, _}} ->
			gun:ws_send(Pid, StreamRef, {close, Code, <<>>}),
			loop(Pid, MRef, StreamRef);
		{gun_ws, Pid, StreamRef, Frame} ->
			gun:ws_send(Pid, StreamRef, Frame),
			loop(Pid, MRef, StreamRef);
		{gun_down, Pid, ws, _, _} ->
			close(Pid, MRef);
		{'DOWN', MRef, process, Pid, normal} ->
			close(Pid, MRef);
		_Msg ->
			close(Pid, MRef)
	end.

update_reports() ->
	{Pid, MRef, StreamRef} = connect("/updateReports?agent=Gun"),
	receive
		{gun_ws, Pid, StreamRef, close} ->
			close(Pid, MRef)
	after 5000 ->
		error(failed)
	end.

connect(Path) ->
	{ok, Pid} = gun:open("fuzzingserver", 9001, #{retry => 0}),
	{ok, http} = gun:await_up(Pid),
	MRef = monitor(process, Pid),
	StreamRef = gun:ws_upgrade(Pid, Path, [], #{compress => true}),
	receive
		{gun_upgrade, Pid, StreamRef, [<<"websocket">>], _} ->
			ok;
		_Msg ->
			terminate(),
			error(failed)
	end,
	{Pid, MRef, StreamRef}.

close(Pid, MRef) ->
	demonitor(MRef),
	gun:close(Pid),
	gun:flush(Pid).

terminate() ->
	ok.
