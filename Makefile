SMLSHARP = smlsharp
all: main
main: common.o ppu.o cpu.o mapper.o main.o common.smi ppu.smi cpu.smi \
 mapper.smi main.smi
	$(SMLSHARP) $(LDFLAGS) -o $@ main.smi $(LIBS)
common.o: common.sml common.smi
	$(SMLSHARP) $(SMLFLAGS) -o $@ -c common.sml
ppu.o: ppu.sml common.smi ppu.smi
	$(SMLSHARP) $(SMLFLAGS) -o $@ -c ppu.sml
cpu.o: cpu.sml common.smi ppu.smi cpu.smi
	$(SMLSHARP) $(SMLFLAGS) -o $@ -c cpu.sml
mapper.o: mapper.sml common.smi ppu.smi cpu.smi mapper.smi
	$(SMLSHARP) $(SMLFLAGS) -o $@ -c mapper.sml
main.o: main.sml common.smi ppu.smi cpu.smi mapper.smi main.smi
	$(SMLSHARP) $(SMLFLAGS) -o $@ -c main.sml
