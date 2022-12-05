-module(docker).

-export([
    get_containers/0,
    get_images/0,
    create_container/1
]).

-record(opt, {
    name :: binary(),
    map_fun :: undefined | function(),
    is_optional :: boolean()
}).

%% Image
%% Hostname
%% Domainname
%% User
%% AttachStdin
%% AttachStdout
%% AttachStderr
%% ExposedPorts
%% Tty
%% OpenStdin
%% StdinOnce
%% Env
%% Cmd
%% Healthcheck
%% ArgsEscaped
%% Volumes
%% WorkingDir
%% Entrypoint
%% NetworkDisabled
%% MacAddress
%% OnBuild
%% Labels
%% StopSignal
%% StopTimeout
%% Shell
%% HostConfig
%% NetworkingConfig
-type optname() :: binary().

-spec get_containers() -> {ok, {StatusLine :: tuple(), Headers :: list(), DecodedJSON :: any()}} | {error, Reason :: any()}.
get_containers() ->
    json_request(get, "/containers/json", []).

-spec get_images() -> {ok, {StatusLine :: tuple(), Headers :: list(), DecodedJSON :: any()}} | {error, Reason :: any()}.
get_images() ->
    json_request(get, "/images/json", []).

-spec create_container(list({optname(), iolist()})) -> {ok, {StatusLine :: tuple(), Headers :: list(), DecodedJSON :: any()}} | {error, Reason :: any()}.
create_container(Opts) ->
    Ret = 
        parse_opts(Opts, [
            #opt{name = <<"Image">>,            is_optional = false},
            #opt{name = <<"Hostname">>,         is_optional = true},
            #opt{name = <<"Domainname">>,       is_optional = true},
            #opt{name = <<"User">>,             is_optional = true},
            #opt{name = <<"AttachStdin">>,      is_optional = true},
            #opt{name = <<"AttachStdout">>,     is_optional = true},
            #opt{name = <<"AttachStderr">>,     is_optional = true},
            #opt{name = <<"ExposedPorts">>,     is_optional = true},
            #opt{name = <<"Tty">>,              is_optional = true},
            #opt{name = <<"OpenStdin">>,        is_optional = true},
            #opt{name = <<"StdinOnce">>,        is_optional = true},
            #opt{name = <<"Env">>,              is_optional = true},
            #opt{name = <<"Cmd">>,              is_optional = true},
            #opt{name = <<"Healthcheck">>,      is_optional = true},
            #opt{name = <<"ArgsEscaped">>,      is_optional = true},
            #opt{name = <<"Volumes">>,          is_optional = true},
            #opt{name = <<"WorkingDir">>,       is_optional = true},
            #opt{name = <<"Entrypoint">>,       is_optional = true},
            #opt{name = <<"NetworkDisabled">>,  is_optional = true},
            #opt{name = <<"MacAddress">>,       is_optional = true},
            #opt{name = <<"OnBuild">>,          is_optional = true},
            #opt{name = <<"Labels">>,           is_optional = true},
            #opt{name = <<"StopSignal">>,       is_optional = true},
            #opt{name = <<"StopTimeout">>,      is_optional = true},
            #opt{name = <<"Shell">>,            is_optional = true},
            #opt{name = <<"HostConfig">>,       is_optional = true},
            #opt{name = <<"NetworkingConfig">>, is_optional = true}
        ]),
    case Ret of
        {ok, Props} ->
            json_request(post, "/containers/create", [], "application/json", jsx:encode(Props));
        {error, _} ->
            Ret
    end.

%% INTERNAL

json_request(Method, Endpoint, ReqHeaders) ->
    case docker_sock:request(Method, Endpoint, ReqHeaders) of
        {ok, {StatusLine, RespHeaders, Data}} ->
            {ok, {StatusLine, RespHeaders, jsx:decode(Data, [return_maps])}};
        Err = {error, _} ->
            Err
    end.

json_request(Method, Endpoint, ReqHeaders, ContentType, Body) ->
    case docker_sock:request(Method, Endpoint, ReqHeaders, ContentType, Body) of
        {ok, {StatusLine, RespHeaders, Data}} ->
            {ok, {StatusLine, RespHeaders, jsx:decode(Data, [return_maps])}};
        Err = {error, _} ->
            Err
    end.

parse_opts(_Opts, []) ->
    {ok, []};
parse_opts(Opts, [Opt|Tail]) ->
    case parse_opts(Opts, Tail) of
        {ok, Acc} ->
            parse_opts_(Opts, Opt, Acc);
        Err = {error, _} ->
            Err
    end.

parse_opts_(Opts, #opt{name = Name, map_fun = MapFun, is_optional = IsOptional}, Acc) ->
    case lists:keyfind(Name, 1, Opts) of
        false when not IsOptional ->
            {error, badopts};
        false ->
            {ok, Acc};
        {Name, Value0} ->
            Value1 =
                case MapFun of
                    undefined ->
                        Value0;
                    _ ->
                        MapFun(Value0)
                end,
            {ok, [{Name, iolist_to_binary(Value1)}|Acc]}
    end.