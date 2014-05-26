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

-define(WITH_DEFAULT_HEADERS(Headers),
        [
         {<<"connection">>, <<"close">>},
         {<<"accept-encoding">>, <<"identity">>},
         %% {<<"accept">>, <<"application/json, text/html">>},
         {<<"accept">>, <<"application/json">>},
         {<<"content-type">>, <<"application/x-www-form-urlencoded">>},
         {<<"pragma">>, <<"no-cache">>},
         {<<"cache-control">>,
          <<"private, max-age: 0, no-cache, must-revalidate">>}
         | Headers]).

-spec request(Method, URL, Headers, Body) ->
  {ok, Status, RespBody} | {error, any()} when Method :: binary() | string(),
                                               URL :: binary() | string(),
                                               Headers :: [proplists:property()],
                                               Body :: string() | binary(),
                                               Status :: non_neg_integer(),
                                               RespBody :: binary().
request(MethodBin, URL, Headers, Body) when is_binary(MethodBin) ->
  Method = list_to_atom(string:to_lower(binary_to_list(MethodBin))),
  request(Method, URL, Headers, Body);
request(Method, URLBin, Headers, Body) when is_binary(URLBin) ->
  request(Method, binary_to_list(URLBin), Headers, Body);
request(Method, URL, Headers, Body) when Method =:= post; Method =:= put ->
  request(Method, {URL, property_bin_to_str(?WITH_DEFAULT_HEADERS(Headers)),
                   "application/x-www-form-urlencode", Body});
request(Method, URL, Headers, _Body) ->
  request(Method, {URL, property_bin_to_str(?WITH_DEFAULT_HEADERS(Headers))}).

request(Method, Request) ->
% pecypc_log:info({req, Method, URL, Body}),
  {ok, {{_, Status, _}, _RespHeaders, RespBody}} = httpc:request(Method, Request, [], []),
  {ok, Status, list_to_binary(RespBody)}.

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

property_bin_to_str(Props) when is_list(Props) ->
  lists:map(fun property_bin_to_str/1, Props);
property_bin_to_str({K, V}) when is_binary(K) ->
  property_bin_to_str({binary_to_list(K), V});
property_bin_to_str({K, V}) when is_binary(V) ->
  property_bin_to_str({K, binary_to_list(V)});
property_bin_to_str({K, V}) when is_list(K), is_list(V) ->
  {K, V}.


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
