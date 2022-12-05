-module(docker_sock).
-export([profile/0, request/3, request/5]).
-export([start/0]).

profile() -> ?MODULE.

request(Method, Endpoint, Headers) ->
    httpc:request(Method, {"http://" ++ Endpoint, Headers}, [], [{body_format, binary}], profile()).

request(Method, Endpoint, Headers, ContentType, Body) ->
    httpc:request(Method, {"http://" ++ Endpoint, Headers, ContentType, Body}, [], [{body_format, binary}], profile()).

start() ->
    DockerHost = 
        case os:getenv("DOCKER_HOST") of
            false -> "/var/run/docker.sock";
            DockerHost_ -> DockerHost_
        end,
    case httpc:start_service([{profile, profile()}]) of
        Ok = {ok, _} ->
            httpc:set_options([{ipfamily, local}, {unix_socket, DockerHost}], profile()),
            Ok;
        Err = {error, _} ->
            Err
    end.