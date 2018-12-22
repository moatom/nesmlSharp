SMLSHARP = smlsharp
LIBS = -lSDL2
all: main
main: common.o ppu.o cpu.o mapper.o sdl2/sdl2.o main.o common.smi ppu.smi \
 cpu.smi mapper.smi sdl2/sdl2.smi main.smi
	$(SMLSHARP) $(LDFLAGS) -o $@ main.smi $(LIBS)
common.o: common.sml common.smi
	$(SMLSHARP) $(SMLFLAGS) -o $@ -c common.sml
ppu.o: ppu.sml common.smi ppu.smi
	$(SMLSHARP) $(SMLFLAGS) -o $@ -c ppu.sml
cpu.o: cpu.sml common.smi ppu.smi cpu.smi
	$(SMLSHARP) $(SMLFLAGS) -o $@ -c cpu.sml
mapper.o: mapper.sml common.smi ppu.smi cpu.smi mapper.smi
	$(SMLSHARP) $(SMLFLAGS) -o $@ -c mapper.sml
sdl2/sdl2.o: sdl2/sdl2.sml sdl2/sdl2.smi
	$(SMLSHARP) $(SMLFLAGS) -o $@ -c sdl2/sdl2.sml
main.o: main.sml common.smi ppu.smi cpu.smi mapper.smi sdl2/sdl2.smi main.smi
	$(SMLSHARP) $(SMLFLAGS) -o $@ -c main.sml
