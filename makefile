#-------------------------------------------------------------------------------
# Copyright 2024 Michael P Williams. All rights reserved.
#-------------------------------------------------------------------------------

EXEC_NAME = cesk-machine

.PHONY: test

build:
	stack build

clean:
	-rm $(EXEC_NAME).prof
	stack clean

run:
	stack run

profile:
	stack --profile run $(EXEC_NAME) --rts-options -p

test:
	stack test
