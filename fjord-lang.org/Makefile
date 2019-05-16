CC=fjc
DIRS=bin

main: bin/Main.js 

bin/Main.js: src/Main.fj
	$(CC) ./ bin/ src/Main.fj

clean:
	rm -rf ./bin

$(shell mkdir -p $(DIRS))
