# Prerequisite: OSDL_SPACE_TOP must be set.


# To understand why taking into account such conditionals requires to specify
# them first, and in each layer, rather than at the end of the last one, refer
# to the section about conditionals at the top of the GNUmakevars.inc of Myriad.


# Overall settings section.

#EXECUTION_TARGET = production

ifndef EXECUTION_TARGET

	# Other possible value: production
	EXECUTION_TARGET = development

endif


ifeq ($(EXECUTION_TARGET),development)

	#$(info Execution target is development)

	ENABLE_DEBUG = true

else ifeq ($(EXECUTION_TARGET),production)

	#$(info Execution target is production)

	ENABLE_DEBUG = false

else

	$(error Invalid OSDL-Space execution target '$(EXECUTION_TARGET)'; expecting either 'development' or 'production')

endif


# Enforce default values:


# Defining (optional) layer-specific flags, typically used by in-code
# conditional compilation primitives (see cond_utils for their management):

# Topics covered by flags:
#  - osdl_space_debug_xx: regarding xx
##
# Defining debug flags (at least the most verbose are by default commented-out):
ifeq ($(ENABLE_DEBUG),true)
	#OSDL_SPACE_DEBUG_FLAGS += -Dosdl_space_debug_xx
endif

# Defining check flags:
ifeq ($(EXECUTION_TARGET),development)
	# Does not exist: OSDL_SPACE_CHECK_FLAGS += -Dosdl_space_check_xx
endif

ERLANG_COMPILER_OPT_BASE += $(OSDL_SPACE_DEBUG_FLAGS) \
							$(OSDL_SPACE_CHECK_FLAGS)



# Useful to target for example the root of the current layer (e.g. to locate the
# relevant, layer-local '_build' directory):
#
ifndef LAYER_TOP
	LAYER_TOP := $(OSDL_SPACE_TOP)
endif


# Project section.


# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME := OSDL-Space
endif


# PROJECT_SHORT_NAME should be defined on a per-project basis.
ifndef PROJECT_SHORT_NAME
	PROJECT_SHORT_NAME := osdl_space
endif


# PROJECT_NAME_FOR_REBAR should be defined on a per-project basis.
ifndef PROJECT_NAME_FOR_REBAR
	PROJECT_NAME_FOR_REBAR := $(PROJECT_SHORT_NAME)
endif


# The uniquely-defined version of this layer, for all uses, including rebar:
OSDL_SPACE_VERSION := 0.0.1


# PROJECT_VERSION should be defined on a per-project basis:
ifndef PROJECT_VERSION
	PROJECT_VERSION := $(OSDL_SPACE_VERSION)
endif


ifndef PACKAGE_NAME
	PACKAGE_NAME := $(PROJECT_NAME)
endif


ifndef PACKAGE_TOP
	PACKAGE_TOP := $(OSDL_SPACE_TOP)
endif


OSDL_SPACE_RELEASE_BASE := osdl-space-$(PROJECT_VERSION)

OSDL_SPACE_RELEASE_ARCHIVE_ZIP := $(OSDL_SPACE_RELEASE_BASE).zip
OSDL_SPACE_RELEASE_ARCHIVE_BZ2 := $(OSDL_SPACE_RELEASE_BASE).tar.bz2
OSDL_SPACE_RELEASE_ARCHIVE_XZ  := $(OSDL_SPACE_RELEASE_BASE).tar.xz


# Source section.
OSDL_SPACE_SRC := $(OSDL_SPACE_TOP)/src

# BEAM path section.
OSDL_SPACE_BEAM := $(OSDL_SPACE_TOP)/src



# See Myriad counterpart variable (MYRIAD_BEAM_DIRS) for explanations:
OSDL_SPACE_BEAM_DIRS := $(OSDL_SPACE_BEAM) $(OSDL_SPACE_TOP)/..


# We rely on Myriad, WOOPER and Traces as well:
BEAM_DIRS += $(OSDL_SPACE_BEAM_DIRS)


ifndef VM_NAME
	VM_NAME := osdl_space_debug
endif


ifndef VM_TEST_NAME
	VM_TEST_NAME := osdl_space_test
endif


# To enforce specific settings such as registration scopes:
INTERNAL_OPTIONS += --config $(OSDL_SPACE_TOP)/conf/osdl-space.config


# For later reuse in the next layer of the software stack:
OSDL_SPACE_PLT_FILE := $(OSDL_SPACE_TOP)/OSDL-Space.plt


# This is the merged PLT of the level just below in the software stack.
# For osdl-space, it is the PLT of 'Traces':
#
ifndef PREDECESSOR_PLT_FILE
	PREDECESSOR_PLT_FILE = $(TRACES_PLT_FILE)
endif


# We define the path to the Traces layer (possibly available as a symbolic
# link).
#
# For the sake of these build-related makefiles, Ceylan dependencies will be
# searched (in that order, the one enforced in otp_utils) in a local _checkouts
# child directory (if checkout directories are used and if the top-level package
# of interest is the current one), otherwise in sibling directories (i.e. whose
# build root is at the same level as this one).
#
# This last case happens notably if not using checkout directories, or if using
# them and if all the packages mentioned here are themselves dependencies (in
# which case they are all sitting together in the _checkouts child directory).
#
# Note also we rely on the names of build roots (e.g. myriad) as opposed to
# project/repository names (e.g. Ceylan-Myriad):
#

# First priority was the expected environment variable; now disabled as this led
# to unfortunate user-based overriding due to .bashrc-related elements (prefer
# symbolic links whenever needed):

#ifneq ($(CEYLAN_TRACES),)
#	TRACES_TOP = $(CEYLAN_TRACES)
#else
	ifneq ($(wildcard $(OSDL_SPACE_TOP)/_checkouts/traces/.),)
		TRACES_TOP = $(OSDL_SPACE_TOP)/_checkouts/traces
	else
		# Second, a sibling one is searched for:
		ifneq ($(wildcard $(OSDL_SPACE_TOP)/../traces/.),)
			TRACES_TOP = $(OSDL_SPACE_TOP)/../traces
		else
			# Otherwise most probably in a 'rebar3 compile' context:
			ifneq ($(wildcard $(OSDL_SPACE_TOP)/_build/default/lib/traces/.),)
				TRACES_TOP = $(OSDL_SPACE_TOP)/_build/default/lib/traces
			else
				FORCE_EVAL := $(error Unable to find the build root of Ceylan-Traces from '$(realpath .)')
			endif
		endif
	endif
#endif

ifndef PREDECESSOR_LAYER_TOPS
	PREDECESSOR_LAYER_TOPS := $(TRACES_TOP)
endif