# Compiler
COBOL_COMPILER = cobc

# Target executable name
TARGET = main

# Source files
SRCS = main.cob

# Rule to build the final executable
final: $(SRCS)
	$(COBOL_COMPILER) -x -o $(TARGET) $(SRCS)

# Clean up compiled files
clean:
	rm -f $(TARGET)


