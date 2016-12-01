# Make (more) human readable html documents from the .md (markdown) files
# From: http://stackoverflow.com/a/8050241
MARKDOWN = pandoc --from markdown_github --to html --standalone
all: $(patsubst %.md,%.html,$(wildcard *.md)) Makefile

clean:
	rm -f $(patsubst %.md,%.html,$(wildcard *.md))
	rm -f *.bak *~

%.html: %.md
	$(MARKDOWN) $< --output $@
