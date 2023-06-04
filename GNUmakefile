OSDL_SPACE_TOP = .


.PHONY: help help-osdl-space register-version-in-header                  \
		register-osdl-space info info-local info-conditionals info-deps


MODULES_DIRS = src doc conf test

# To override the 'all' default target with a parallel version:
BASE_MAKEFILE := true


# Default target:
help: help-intro help-osdl-space

help-osdl-space:
	@cd $(TRACES_TOP) && $(MAKE) -s help-traces


register-version-in-header:
	@if [ -z "$(VERSION_FILE)" ]; then \
	echo "Error, no version file defined." 1>&2; exit 52; else \
	$(MAKE) -s register-osdl-space; fi


register-osdl-space:
	@echo "-define( osdl_space_version, \"$(OSDL_SPACE_VERSION)\" )." >> $(VERSION_FILE)


# Useful to extract internal layout for re-use in upper layers:
list-beam-dirs:
	@for d in $(OSDL_SPACE_BEAM_DIRS); do echo $$(readlink -f $$d); done


stats:
	@$(MAKE_CODE_STATS) $(OSDL_SPACE_TOP)


info: info-local


info-local:
	@echo "REBAR3_EXEC = $(REBAR3_EXEC)"


info-conditionals:
	@echo "OSDL_SPACE_DEBUG_FLAGS = $(OSDL_SPACE_DEBUG_FLAGS)"
	@echo "OSDL_SPACE_CHECK_FLAGS = $(OSDL_SPACE_CHECK_FLAGS)"


# Typically useful to know the software context for continuous integration:
info-context: info-platform info-versions info-source-layout


info-versions:
	@echo "OSDL_SPACE_VERSION  = $(OSDL_SPACE_VERSION)"
	@echo "MYRIAD_VERSION      = $(MYRIAD_VERSION)"
	@echo "WOOPER_VERSION      = $(WOOPER_VERSION)"
	@echo "TRACES_VERSION      = $(TRACES_VERSION)"


info-deps:
	@echo "OSDL_SPACE_TOP = $(OSDL_SPACE_TOP) (i.e. $$(realpath $(MYRIAD_TOP)))"
	@echo "MYRIAD_TOP     = $(MYRIAD_TOP) (i.e. $$(realpath $(MYRIAD_TOP)))"
	@echo "WOOPER_TOP     = $(WOOPER_TOP) (i.e. $$(realpath $(WOOPER_TOP)))"
	@echo "TRACES_TOP     = $(TRACES_TOP) (i.e. $$(realpath $(TRACES_TOP)))"


include $(OSDL_SPACE_TOP)/GNUmakesettings.inc
