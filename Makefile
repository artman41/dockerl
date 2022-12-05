PROJECT = dockerl
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS += jsx
DEPS += cowboy
DEPS += lager

LOCAL_DEPS += inets

BUILD_DEPS += relx

dep_cowboy = git https://github.com/ninenines/cowboy.git 2.9.0
dep_lager = git https://github.com/erlang-lager/lager.git 3.9.2

SHELL_OPTS += -eval 'application:ensure_all_started($(PROJECT)).'

ERLC_OPTS += +'{parse_transform, lager_transform}'

include erlang.mk