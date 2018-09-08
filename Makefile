PROJECT = cm_heat_equation
PROJECT_DESCRIPTION = Calculation of the heat equation in grid nodes. Plotting.
PROJECT_VERSION ?= $(shell git describe --tags --always | sed 's/-/./g')
PROJECT_MOD = cm_heat_equation_app

SHELL_ERL = erl -name $(PROJECT) -eval 'application:ensure_all_started($(PROJECT))'

include erlang.mk
