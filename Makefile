all: asm interpreter

asm: Assembler.hs Engine.hs
	ghc -o asm Assembler

interpreter: Interpreter.hs Engine.hs
	ghc -o interpreter Interpreter

clean:
	rm *.hi *.o asm interpreter
