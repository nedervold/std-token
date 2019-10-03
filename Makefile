.PHONY : all clean hindent

HINDENT = hindent --line-length 76 --sort-imports

all :
	stack build

hindent :
	find src -name '*.hs' -exec $(HINDENT) \{} \;

clean : hindent
	stack clean
	find . -name '*~' -delete
	find . -name '#*' -delete
