-module(basic_driver).

-export([test/0, test/1]).

test(Message) when is_list(Message) ->
  {ok, P} = load_driver(),
  port_command(P, list_to_binary(Message)),
  receive
    Data ->
      io:format("Data: ~p~n", [Data])
  after 100 ->
      io:format("Received nothing!~n")
  end,
  port_close(P).

test() ->
  {ok, P} = load_driver(),
  Term = <<"abc">>,
  Size = size(Term),
  port_command(P, [<<Size:32>>, Term]),
  receive
    Data ->
      io:format("Data: ~p~n", [Data])
  after 100 ->
      io:format("Received nothing!~n")
  end,
  port_close(P).

%% Private functions
load_driver() ->
  SearchDir = filename:join([filename:dirname(code:which(basic_driver)), "..", "priv"]),
  case erl_ddll:load(SearchDir, "basic_drv") of
    ok ->
      {ok, open_port({spawn, 'basic_drv'}, [binary])};
    Error ->
      Error
  end.
