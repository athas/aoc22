DATA=data/1.input data/2.input data/3.input data/4.input data/5.input data/6.input data/7.input data/8.input data/9.input

all: $(DATA)

data:
	mkdir -p data

data/%.input: %.input data
	cat $< | ./txt2fut.py > $@
