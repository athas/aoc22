DATA=data/1.input

all: $(DATA)

data:
	mkdir -p data

data/%.input: %.input data
	cat $< | ./txt2fut.py > $@
