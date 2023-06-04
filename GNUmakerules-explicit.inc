# Here are gathered explicit (immediate, static) rules for OSDL-Space.
#
# See GNUmakerules-automatic.inc for their automatic (generic, pattern-based)
# counterparts.


# Prerequisite: OSDL_SPACE_TOP must be set.

.PHONY: default-osdl-space-rule create-osdl-space-checkout



# The first non-generic, therefore default, rule:
default-osdl-space-rule: all


# Create a _checkouts directory in which all Ceylan dependencies for the current
# layer are referenced through a symlink.
#
create-osdl-space-checkout:
	@echo "  Creating a checkout directory for OSDL-Space"
	@$(MAKE) -s create-traces-checkout
	@cd _checkouts && ln -sf ../$(TRACES_TOP)
