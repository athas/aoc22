DATA=data/1.input data/2.input data/3.input data/4.input data/5.input data/6.input data/7.input data/8.input data/9.input data/10.input data/11.input data/12.input data/13.input data/14.input data/15.input data/16.input data/17.input data/18.input data/19.input

all: $(DATA)

data/%.input: inputs/%.input
	@mkdir -p data
	cat $< | ./txt2fut.py > $@

clean:
	rm -rf data
