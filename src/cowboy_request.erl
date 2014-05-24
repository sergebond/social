%%
%%------------------------------------------------------------------------------
%% !!! Hackish reuse of cowboy_client undocumented interface !!!
%%------------------------------------------------------------------------------
%%

-module(cowboy_request).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-export([get_json/2]).
-export([post_for_json/2]).
-export([request/4]).
-export([urlencode/1]).
-export([make_uri/3]).

request(Method, URL, Headers, Body) ->
% pecypc_log:info({req, Method, URL, Body}),
  {ok, GunPid} = gun:open(URL, 80),
  Tag = monitor(process, GunPid),
  StreamRef = gun:request(GunPid, Method, URL, [
      {<<"connection">>, <<"close">>},
      {<<"accept-encoding">>, <<"identity">>},
      % {<<"accept">>, <<"application/json, text/html">>},
      {<<"accept">>, <<"application/json">>},
      {<<"content-type">>, <<"application/x-www-form-urlencoded">>},
      {<<"pragma">>, <<"no-cache">>},
      {<<"cache-control">>,
          <<"private, max-age: 0, no-cache, must-revalidate">>}
      | Headers
    ], Body),
  {ok, Status, _Headers, Body} = receive_response(GunPid, Tag, StreamRef),
  io:format("Status: ~p, Body: ~p", [Status, Body]),
  {ok, Status, Body}.

receive_response(Pid, Tag, StreamRef) ->
  receive
    {'DOWN', Tag, _, _, Reason} ->
      exit(Reason);
    {gun_response, Pid, StreamRef, fin, Status, Headers} ->
      {ok, Status, Headers, no_data};
    {gun_response, Pid, StreamRef, nofin, Status, Headers} ->
      {ok, Status, Headers, receive_data(Pid, Tag, StreamRef)}
  after 1000 ->
      exit(timeout)
  end.

receive_data(Pid, Tag, StreamRef) ->
  receive_data(Pid, Tag, StreamRef, []).

receive_data(Pid, Tag, StreamRef, DataAcc) ->
  receive
    {'DOWN', Tag, _, _, Reason} ->
      {error, {incomplete, Reason}};
    {gun_data, Pid, StreamRef, nofin, Data} ->
      receive_data(Pid, Tag, StreamRef, [Data | DataAcc]);
    {gun_data, Pid, StreamRef, fin, Data} ->
      {ok, lists:reverse([Data | DataAcc])}
  after 1000 ->
      {error, timeout}
  end.

make_uri(Scheme, Host, Path) ->
  << Scheme/binary, "://", Host/binary, Path/binary >>.

urlencode(Bin) when is_binary(Bin) ->
  cowboy_http:urlencode(Bin);
urlencode(Atom) when is_atom(Atom) ->
  urlencode(atom_to_binary(Atom, latin1));
urlencode(Int) when is_integer(Int) ->
  urlencode(list_to_binary(integer_to_list(Int)));
urlencode({K, undefined}) ->
  << (urlencode(K))/binary, $= >>;
urlencode({K, V}) ->
  << (urlencode(K))/binary, $=, (urlencode(V))/binary >>;
urlencode(List) when is_list(List) ->
  binary_join([urlencode(X) || X <- List], << $& >>).

binary_join([], _Sep) ->
  <<>>;
binary_join([H], _Sep) ->
  << H/binary >>;
binary_join([H | T], Sep) ->
  << H/binary, Sep/binary, (binary_join(T, Sep))/binary >>.

parse(JSON) ->
  case jsx:decode(JSON, [{error_handler, fun(_, _, _) -> {error, badarg} end}])
  of
    {error, _} ->
      {ok, cowboy_http:x_www_form_urlencoded(JSON)};
    {incomplete, _} ->
      {ok, []};
    Hash ->
      {ok, Hash}
  end.

get_json(URL, Data) ->
  case request(<<"GET">>, <<
        URL/binary, $?, (urlencode(Data))/binary >>, [], <<>>)
  of
    {ok, 200, JSON} -> parse(JSON);
    _ -> {error, badarg}
  end.

post_for_json(URL, Data) ->
  case request(<<"POST">>, URL, [
      {<<"content-type">>, <<"application/x-www-form-urlencoded">>}
    ], urlencode(Data))
  of
    {ok, 200, JSON} -> parse(JSON);
    _Else -> {error, badarg}
  end.
