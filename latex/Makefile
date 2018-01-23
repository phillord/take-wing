$(info var is $(MAKECMDGOALS))

## what ever we called, don't do it here
ifndef MAKECMDGOALS
	MAKECMDGOALS=all
endif

$(info var is $(MAKECMDGOALS))



## what ever we called, don't do it here
$(MAKECMDGOALS):
	$(MAKE) -C .. $(MAKECMDGOALS)
