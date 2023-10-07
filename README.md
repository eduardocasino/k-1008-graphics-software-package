# K-1008 Graphics Software Package

## Introduction

This repository contains the manual facsimile, the sources in their original and 64tass assembly formats, listings and assembled programs (in Intel HEX and MOS Papertape formts) that came with MTU's Graphics Software Package for the K-1008 "Visable" memory card for the KIM-1.

This is a mix of OCR and manual typing, so there will probably be errors. Please open an issue if you detect any mistake.

The manual, sources and listings should be exact copies of the originals, including bugs, typos or grammatical errors. I do not intend to make any ammendments.

## Contents

```bash
├── LICENSE.md
├── README.md
├── 64tass
│   ├── Makefile
│   ├── sdtxt.asm
│   ├── swirl.asm
│   ├── vmlif.asm
│   └── vmsup.asm
├── binaries
│   ├── sdtxt.hex
│   ├── sdtxt.pap
│   ├── swirl.hex
│   ├── swirl.pap
│   ├── vmlif.hex
│   ├── vmlif.pap
│   ├── vmsup.hex
│   └── vmsup.pap
├── listings
│   ├── sdtxt.lst
│   ├── swirl.lst
│   ├── vmlif.lst
│   └── vmsup.lst
├── manual
│   ├── life_graphics.ods
│   ├── manual.odt
│   └── manual.pdf
└── sources
    ├── sdtxt.s
    ├── swirl.s
    ├── vmlif.s
    └── vmsup.s
```

## Licensing

Although it probably doesn't have any significance nowadays, as this software is meant for a very obsolete and rare piece of hardware, this material is copyrighted by MICRO TECHNOLOGY UNLIMITED, a company that is [still in business](http://www.mtu.com/catalog/index.php). I've put it here just to be of assistance to the owners of a K-1008 card.

See the original LICENSE.md file for details.

I claim no ownership on the contents of this repository.

## Acknowledgements

* Hans Otten and his [Retro Computing blog](http://retro.hansotten.nl/). Tons of information about the K-1008 card, including the Graphics Software Package and thorough proofreading and testing.