-module(dockerl_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([get_mimetypes/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    docker_sock:start(),
    SupFlags = #{
        strategy => one_for_one, 
        intensity => 1, 
        period => 5
    },
    Children = [
        #{
            id => docker_srv,
            start => {docker_srv, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker
        },
        cowboy_child_spec(http, [{port, 8080}], #{
            env => #{
                dispatch => cowboy_router:compile([
                    {'_', [
                        {"/api/[...]", web_handler, []},
                        {"/", cowboy_static, {priv_file, dockerl, "www/index.html"}},
                        {"/[...]", cowboy_static, {priv_dir, dockerl, "www/", [{mimetypes, ?MODULE, get_mimetypes}]}}
                    ]}
                ])
            }
        })
    ],
    {ok, {SupFlags, Children}}.

cowboy_child_spec(Ref, TransOpts, ProtoOpts) ->
    ranch:child_spec(Ref, ranch_tcp, TransOpts, cowboy_clear, ProtoOpts).

get_mimetypes(Path) ->
    Mimetype = 
        case filename:extension(Path) of
            <<".mjs">> -> 
                {<<"application">>, <<"javascript">>, []};
            _ -> 
                cow_mimetypes:web(Path)
        end,
    Mimetype.