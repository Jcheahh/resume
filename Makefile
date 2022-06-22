filename = jack.pdf
local-generate:
	stack main.hs | wkhtmltopdf --dpi 350 - $(filename)

generate:
	stack main.hs | xvfb-run -- wkhtmltopdf --dpi 350 - $(filename)

clean:
	rm -f *.pdf
