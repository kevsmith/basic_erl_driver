%% Copyright (c) 2009 Hypothetical Labs, Inc.

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(basic_driver).

-export([start/0, stop/0, echo/1, reverse/1]).
-export([compress/1, decompress/1]).

start() ->
  ok = load_driver().

stop() ->
  erl_ddll:unload("basic_drv").

echo(Text) ->
  call_driver("ec", [Text]).

reverse(Text) ->
  call_driver("rv", [Text]).

compress(Text) ->
  call_driver("sc", [Text]).

decompress(Text) ->
  call_driver("sd", [Text]).

%% Private functions
call_driver(Command, Args) ->
  P = open_port({spawn, basic_drv}, [binary]),
  Marshalled = driver_comm:pack(Command, Args),
  port_command(P, Marshalled),
  Result = receive
             Response ->
               Response
           after 100 ->
               {error, timeout}
           end,
  port_close(P),
  Result.

load_driver() ->
  SearchDir = filename:join([filename:dirname(code:which(?MODULE)), "..", "priv"]),
  erl_ddll:load(SearchDir, "basic_drv").
