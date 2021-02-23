DOTNET=dotnet

all:

REPL:
	$(DOTNET) run --project REPL/REPL.fsproj
.PHONY: REPL

test:
	$(DOTNET) test  --nologo -l "console;v=d"
.PHONY: test
