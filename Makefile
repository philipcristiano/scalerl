PROJECT = scalerl
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = swaggerl kuberlnetes lager
SHELL_DEPS = sync

dep_swaggerl = git git@github.com:philipcristiano/swaggerl.git master
dep_kuberlnetes = git git@github.com:philipcristiano/kuberlnetes.git actions
dep_sync = git https://github.com/rustyio/sync.git master

SHELL_OPTS = -eval 'application:ensure_all_started(scalerl).' -config sys

.PHONY: docker
docker:
	docker build . -t scalerl

include erlang.mk
