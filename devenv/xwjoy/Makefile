TARGET=xwjoy.com

SOURCES=xwjoy.asm
OBJS=$(SOURCES:.asm=.obj)

all: $(TARGET) dbgxwjoy.com

clean:
    -rm $(TARGET) $(OBJS:.com=.lst) $(TARGET:.com=.map) $(OBJS) $(OBJS:.obj=.lst) dbgxwjoy.*

.PHONY: clean

$(TARGET): $(SOURCES)
    ml /AT /Fl$(TARGET:.com=.lst) $?

dbgxwjoy.com: $(SOURCES)
    ml /AT /DDEBUGMODE=1 /Fl$(@:.com=.lst) /Fe$@ $?

