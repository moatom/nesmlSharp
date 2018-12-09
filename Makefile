SMLSHARP = smlsharp
all: main
main: cpu.o mapper.o main.o cpu.smi mapper.smi main.smi
	$(SMLSHARP) $(LDFLAGS) -o $@ main.smi $(LIBS)
cpu.o: cpu.sml cpu.smi
	$(SMLSHARP) $(SMLFLAGS) -o $@ -c cpu.sml
mapper.o: mapper.sml cpu.smi mapper.smi
	$(SMLSHARP) $(SMLFLAGS) -o $@ -c mapper.sml
main.o: main.sml cpu.smi mapper.smi main.smi
	$(SMLSHARP) $(SMLFLAGS) -o $@ -c main.sml
