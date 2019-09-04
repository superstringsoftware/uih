# the compiler to use
# export CC := /usr/local/opt/llvm/bin/clang
export CC := clang

# compiler flags:
#  -g    adds debugging information to the executable file
#  -Wall turns on most, but not all, compiler warnings
CFLAGS  = -g -Wall -I/usr/local/include/SDL2 -lSDL2 -v

#files to link:
#LDFLAGS += -L/usr/local/Cellar/sdl2/2.0.9/lib -Wl,-rpath,/usr/local/opt/llvm/lib, -lSDL2
#CPPFLAGS += -I/usr/local/opt/llvm/include

# the name to use for both the target source file, and the output file:
TARGET = main

all: $(TARGET)

$(TARGET): src/$(TARGET).c
	$(CC) $(CFLAGS) -o bin/$(TARGET) src/$(TARGET).c $(CPPFLAGS)

llvm:
	$(CC) -S -emit-llvm -o bin/$(TARGET).ll src/$(TARGET).c

clean:
	\rm -f bin/*; rm -rf bin/main.dSYM
