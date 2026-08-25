# nand-curry
## Project Overview
`nand-curry` is an attempt to simulate an entire von Neumann computer (as detailed in the book 'But How Do It Know?'—J Clark Scott) logically in Haskell, starting with only the definition of a single NAND gate as a pure function, then currying and composing functions defined in terms of that one NAND function to build an entire computer. A more complete introduction can be found at https://stefanciutac.github.io/stefanciutac_devlog/posts/nand-curry-introduction/.

## Documentation
Comprehensive documentation of the design decisions made can be accessed at https://stefanciutac.github.io/stefanciutac_devlog/posts/.

## Project Status
- the RAM/main memory module has been implemented, and has passed initial testing
- the ALU is in progress, with the `Logic.hs` module actively under development

## Prerequisites
Should you wish to experiment with the modules I have written, you must have on your system:
- a Haskell compiler, such as GHC (the installer for which can be obtained at https://www.haskell.org/ghcup/)

## Modules
- `Gates.hs` contains the basic definitions of each gate, all in terms of the NAND gate function (read more at: https://stefanciutac.github.io/stefanciutac_devlog/posts/nand_curry-gates-module/nand-curry-gates-module/)
- `Register.hs` defines the bit as a pure, functional implementation of a D-type latch, and other memory units—as well as decoders of different arities (read more at: https://stefanciutac.github.io/stefanciutac_devlog/posts/nand-curry-register-module/nand-curry-register-module/)
- `RAM.hs` defines the main memory as a pure function
- `Logic.hs` [WIP] defines the components of the ALU
