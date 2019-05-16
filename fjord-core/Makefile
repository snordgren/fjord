CC=fjc
DIRS=bin

main: bin/core/Glue.js bin/Core.js 

bin/core/Glue.js: src/core/Glue.d.fj src/core/Glue.js
	mkdir -p bin/core & cp src/core/Glue.js bin/core/Glue.js & cp src/core/Glue.d.fj bin/core/Glue.d.fj

bin/Core.js: bin/core/Glue.js src/Core.fj
	$(CC) bin bin/ src/Core.fj

clean:
	rm -rf ./bin

$(shell mkdir -p $(DIRS))
