-module(web_handler).
-behaviour(cowboy_handler).

-export([init/2]).
-export([terminate/3]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path_info(Req0),
    Headers = cowboy_req:headers(Req0),
    handle(Method, Path, Headers, Req0, Opts).

terminate(_Reason, _Req, _State) ->
    ok.

%% INTERNAL

handle(Method, [<<"socket">>, <<"direct">> | Tail], Headers, Req0, Opts) ->
    Path = lists:flatmap(fun erlang:binary_to_list/1, lists:join(<<"/">>, [<<>>|Tail])),

    AtomMethod = 
        case Method of
            <<"GET">> -> get;
            <<"POST">> -> post;
            <<"PUT">> -> put;
            <<"UPDATE">> -> update;
            <<"DELETE">> -> delete;
            <<"HEAD">> -> head;
            <<"OPTIONS">> -> options;
            <<"CONNECT">> -> connect;
            <<"TRACE">> -> trace;
            <<"PATCH">> -> patch
        end,

    ReqHeaders = [{binary_to_list(Field), Value} || {Field, Value} <- maps:to_list(Headers)],

    {Req1, F, A} =
        case AtomMethod of
            get ->
                {Req0, fun docker_sock:request/3, [AtomMethod, Path, ReqHeaders]};
            _ ->
                {Data, Req} = read_full_body(Req0),
                ContentType = maps:get(<<"content-type">>, Headers, <<"application/x-www-form-urlencoded">>),
                {Req, fun docker_sock:request/5, [AtomMethod, Path, ReqHeaders, binary_to_list(ContentType), iolist_to_binary(Data)]}
        end,
    lager:info("Fun: ~p, Args: ~p~n", [F, A]),
    Req2 = 
        case erlang:apply(F, A) of
            {ok, {{_, HTTPCode, _}, DockerHeaders, RespBody}} ->
                RespHeaders = maps:from_list([{list_to_binary(K), list_to_binary(V)} || {K,V} <- DockerHeaders]),
                lager:info("HTTPCode: ~p, RespHeaders: ~p, RespBody: ~p~n", [HTTPCode, RespHeaders, RespBody]),
                cowboy_req:reply(HTTPCode, RespHeaders, RespBody, Req1);
            Err = {error, _} ->
                cowboy_req:reply(500, #{}, io_lib:format("Request failed with error ~p~n", [Err]), Req1)
        end,
    {ok, Req2, Opts};
handle(Method, Path, Headers, Req0, Opts) ->
    lager:info("Method: ~p, Path: ~p, Headers: ~p~n", [Method, Path, Headers]),
    Req1 = cowboy_req:reply(404, Req0),
    {ok, Req1, Opts}.

read_full_body(Req) ->
    read_full_body_(Req, []).

read_full_body_(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} ->
            {[Acc, Data], Req};
        {more, Data, Req} ->
            read_full_body_(Req, [Acc, Data])
    end.