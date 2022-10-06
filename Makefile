
convert: Happy-Birthday What-Child-Is-This

Happy-Birthday:
	racket racket/musescore-guitar-open-lines-lib/main.rkt experiment/Happy-Birthday/Happy_Birthday.mscx

What-Child-Is-This:
	racket racket/musescore-guitar-open-lines-lib/main.rkt experiment/What-Child-Is-This/What_Child_Is_This.mscx

clean:
	rm -rf experiment/Happy-Birthday/conversion experiment/What-Child-Is-This/conversion
