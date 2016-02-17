-module(sockets).

-export([get_sockets/0]).

-record(socket_info_ex, {name, links, id, port, address, peer_address, peer_port, owner, input, output}).

get_sockets() ->
  SockPorts0 = [{SockPort, erlang:port_info(SockPort)} || SockPort <- erlang:ports()],
  F = fun({_Sock, Info}) ->
          lists:keysearch(name, 1, Info) == {value, {name, "tcp_inet"}}
      end,
  SockPorts = lists:filter(F, SockPorts0),
  SockPortsInfo =
    lists:map(
      fun({Sock,Info}) ->
          {socket_info, Name, Links, ID, Owner, Input, Output} = get_socket_info(Info),
          {Address,Port} =
            case catch inet:sockname(Sock) of
              {ok, {IP, PortNo}} -> {print_ip(IP), PortNo};
              _NoSock            -> {"-", -1}
            end,
          {PeerAddress, PeerPort} =
            case catch inet:peername(Sock) of
              {ok, {PIP, PPortNo}} -> {print_ip(PIP), PPortNo};
              _PNoSock             -> {"-", -1}
            end,
          {socket_info_ex, Name, Links, ID, Port, Address, PeerAddress, PeerPort, Owner, Input, Output}
      end,
      SockPorts),
  GoodSocks =
    lists:filter(
      fun(#socket_info_ex{port = -1, peer_port = -1}) -> false;
         (_)                                          -> true
      end,
      SockPortsInfo),
  GoodSocks.

get_socket_info(Info) ->
  get_socket_info(Info, {socket_info, "undefined", [], -1, undefined, -1, -1}).

get_socket_info([], Res) ->
  Res;
get_socket_info([{name, Name} | More], {socket_info, _Name, Links, ID, Connected, Input, Output}) ->
  get_socket_info(More, {socket_info, Name, Links, ID, Connected, Input, Output});
get_socket_info([{links, Links} | More], {socket_info, Name, _Links, ID, Connected, Input, Output}) ->
  get_socket_info(More, {socket_info, Name, Links, ID, Connected, Input, Output});
get_socket_info([{id, ID} | More], {socket_info, Name, Links, _ID, Connected, Input, Output}) ->
  get_socket_info(More, {socket_info, Name, Links, ID, Connected, Input, Output});
get_socket_info([{connected, Connected} | More], {socket_info, Name, Links, ID, _Connected, Input, Output}) ->
  get_socket_info(More, {socket_info, Name, Links, ID, Connected, Input, Output});
get_socket_info([{input, Input} | More], {socket_info, Name, Links, ID, Connected, _Input, Output}) ->
  get_socket_info(More, {socket_info, Name, Links, ID, Connected, Input, Output});
get_socket_info([{output, Output} | More], {socket_info, Name, Links, ID, Connected, Input, _Output}) ->
  get_socket_info(More, {socket_info, Name, Links, ID, Connected, Input, Output});
get_socket_info([_H | More], Res) ->
  get_socket_info(More, Res).

print_ip({B1, B2, B3, B4}) ->
  integer_to_list(B1) ++ "." ++
    integer_to_list(B2) ++ "." ++
    integer_to_list(B3) ++ "." ++
    integer_to_list(B4);
print_ip(Other) ->
  lists:flatten(io_lib:format("~p", [Other])).
