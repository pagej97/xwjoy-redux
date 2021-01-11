TARGET=xwjoy.com

SOURCES=xwjoy.asm
OBJS=$(SOURCES:.asm=.obj)

all: $(TARGET)

clean:
    -rm $(TARGET) $(OBJS:.com=.lst) $(TARGET:.com=.map) $(OBJS) $(OBJS:.obj=.lst)

.PHONY: clean

$(TARGET): $(SOURCES)
    ml /AT /Fl$(TARGET:.com=.lst) $?

