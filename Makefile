TARGET=xwjoy.com

SOURCES=xwjoy.asm
OBJS=$(SOURCES:.asm=.obj)

all: $(TARGET)

clean:
    -rm $(TARGET) $(OBJS:.com=.lst) $(TARGET:.com=.map) $(OBJS) $(OBJS:.obj=.lst)

.PHONY: clean

$(TARGET): $(SOURCES)
    masm /AT /Fl$(TARGET:.com=.lst) $?

#.asm.obj:
#    echo masm $<
#    masm /AT /c $<

#$(OBJS): $(SOURCES)

#$(TARGET): $(OBJS)
#    link $(OBJS),$(TARGET),$(TARGET:.com=.map),,,

