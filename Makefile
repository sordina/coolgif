all:
	mkdir -p frames
	runhaskell coolgif.hs
	convert -delay 0 frames/*.pgm coolgif.gif

clean:
	rm -rf frames
