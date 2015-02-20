DIRS = abalone_ctl \
	   abalone_dbmigrate \
	   abalone_example_ai \
	   abalone_gameoperator \
	   abalone_httpd \
	   abalone_relayd \

install:
	$(foreach dir, $(DIRS), cd $(PWD)/$(dir) && go install; )
