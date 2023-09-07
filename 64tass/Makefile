BINDIR = ../binaries

AFLAGS = -a

all : $(BINDIR)/swirl.pap \
      $(BINDIR)/vmlif.pap \
  	  $(BINDIR)/sdtxt.pap \
  	  $(BINDIR)/vmsup.pap

clean:
	rm -f $(BINDIR)/*.hex $(BINDIR)/*.pap

$(BINDIR)/vmlif.pap : AFLAGS += -Wno-wrap-addr

$(BINDIR)/%.pap : %.asm
	64tass $(AFLAGS) -o $(@D)/$(*F).hex --intel-hex $<
	srec_cat $(@D)/$(*F).hex -Intel -Output $@ -MOS_Technologies
