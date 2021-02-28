.PHONY: all

RST := $(wildcard *.rst)
HTML := $(RST:.rst=.html)

%.html: %.rst
	pandoc --to html $< > $@

all: $(HTML)
