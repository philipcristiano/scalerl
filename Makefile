PROJECT = scalerl
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

BUILD_DEPS = elvis_mk
DEPS = swaggerl kuberlnetes lager
SHELL_DEPS = sync

dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 1.0.0
dep_swaggerl = git https://github.com/philipcristiano/swaggerl.git in-cluster
dep_kuberlnetes = git https://github.com/philipcristiano/kuberlnetes.git actions
dep_sync = git https://github.com/rustyio/sync.git master

DEP_PLUGINS = elvis_mk

SHELL_OPTS = -eval 'application:ensure_all_started(scalerl).' -config sys

.PHONY: docker
docker:
	docker build . -t scalerl

include erlang.mk
