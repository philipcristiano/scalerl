PROJECT = scalerl
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = kuberlnetes lager

dep_kuberlnetes = git git@github.com:philipcristiano/kuberlnetes.git master

SHELL_OPTS = -eval 'application:ensure_all_started(scalerl).' -config sys

include erlang.mk
