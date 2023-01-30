.PHONY : all
all :
	stack build

.PHONY : clean
clean :
	find . -name '*~' -delete
	find . -name '#*' -delete
	stack clean
