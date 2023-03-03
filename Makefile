PROJECT = dockerl
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS += jsx
DEPS += cowboy
DEPS += lager
DEPS += cowlib

LOCAL_DEPS += inets

BUILD_DEPS += relx

dep_cowboy = git https://github.com/ninenines/cowboy.git 2.9.0
dep_lager = git https://github.com/erlang-lager/lager.git 3.9.2

SHELL_OPTS += -config config/sys.config
SHELL_OPTS += -args_file config/vm.args
SHELL_OPTS += -eval 'application:ensure_all_started($(PROJECT)).'

ERLC_OPTS += +'{parse_transform, lager_transform}'

noop=
space = $(noop) $(noop)
comma = ,

define JOIN_WORDS
$(subst $(space),$(2),$(1))
endef

include erlang.mk

deps::
	$(gen_verbose) git checkout -- .vscode/dockerl.code-workspace && \
	file=`mktemp` && \
	jq '.folders |= . + [$(call JOIN_WORDS,$(foreach depdir,$(ALL_DEPS_DIRS),{"name":"dep:$(notdir $(depdir))","path":"$(depdir)"}),$(comma))]' .vscode/dockerl.code-workspace > $$file && \
	mv $$file .vscode/dockerl.code-workspace;