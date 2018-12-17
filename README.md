# game-of-life

Whoa! I was digging through my old floppy disk archive and came across an old implementation of Conway's Game of Life I did, circa 1994. So here it is.

Crazy tidbits:
- Its in Fortran 90 (!). Yes Fortran windows apps do exist. I do not remember what possessed me to do it in Fortran since I knew C/C++/many other languages at the time.
- man was I young when I built this
- My ideas about UI design are accordingly antiquated
- Despite being ~25 yrs old code, the EXE (added here) still runs just fine in Wine on my current Fedora 29 laptop. Drag and drop (to/from pattern selector to board) does not seem to work :(
- Not sure how to rebuild this; it was originally based in a windowing toolkit known as WINTERATOR (still available! see https://www.absoft.com/products/winteracter-gui-toolset/)
- Pretty naive implementation; I was aware of better algorithms like hashlife (see https://en.wikipedia.org/wiki/Hashlife) but did not use these for unknown reasons (maybe cause hashlife is less suitable for the sort of interactive app this is?)
