DATA=data/1.input data/2.input data/3.input data/4.input data/5.input data/6.input data/7.input data/8.input data/9.input data/10.input data/11.input data/12.input data/13.input

all: $(DATA)

data/%.input: %.input
	@mkdir -p data
	cat $< | ./txt2fut.py > $@

clean:
	rm -rf data
