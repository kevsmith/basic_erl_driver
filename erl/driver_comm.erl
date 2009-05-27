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

-module(driver_comm).

-export([pack/2]).

pack(Cmd, Terms) when length(Cmd) == 2 ->
  NewTerms = lists:foldr(fun(T, Acc) -> marshal_data(T, Acc) end, [], Terms),
  list_to_binary(lists:flatten([[Cmd], [NewTerms]])).

%% Internal functions
marshal_data(Term, Acc) when is_integer(Term) ->
  [<<Term:32>>|Acc];
marshal_data(Term, Acc) when is_list(Term) ->
  marshal_data(list_to_binary(Term), Acc);
marshal_data(Term, Acc) when is_binary(Term) ->
  S = size(Term),
  [[<<S:32>>, Term]|Acc].
