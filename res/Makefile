DOTS = $(wildcard *.dot)
PDF = $(DOTS:.dot=.pdf)

%.pdf: %.dot
	dot -Tpdf $< -o $@

all: $(PDF)

clean:
	rm -rf $(PS)

.PHONY: all clean