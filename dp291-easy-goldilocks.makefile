LINE_IDX  ?= 0

ifneq ($(GOLDI_DATA),)
	curr_data  := $(wordlist 1, 2, $(FILE_LINES))
	rest_data  := $(wordlist 3, 999, $(FILE_LINES))
else
	GOLDI_DATA := $(wordlist 1, 2, $(FILE_LINES))
	rest_data  := $(wordlist 3, 999, $(FILE_LINES))
endif

.PHONY : all
all :
ifneq ($(curr_data),)
	@if [ $(word 1, $(GOLDI_DATA)) -lt $(word 1, $(curr_data))		\
	   -a $(word 2, $(GOLDI_DATA)) -gt $(word 2, $(curr_data)) ] ; then	\
		echo $(LINE_IDX);						\
	fi
endif
ifneq ($(rest_data),)
	@$(MAKE) -s GOLDI_DATA="$(GOLDI_DATA)" LINE_IDX=$(shell echo $$(($(LINE_IDX)+1))) FILE_LINES="$(rest_data)"
endif

# See: https://www.reddit.com/r/dailyprogrammer/comments/5bn0b7/20161107_challenge_291_easy_goldilocks_bear/

# With following content for challenge.txt:
#   100 80
#   30 50
#   130 75
#   90 60
#   150 85
#   120 70
#   200 200
#   110 100

# Run with: make -f dp291-easy-goldilocks.makefile FILE_LINES=\"`tr '\n' ' ' < challenge.txt`\"
