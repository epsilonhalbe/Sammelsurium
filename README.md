# Sammelsurium

This is a collection of toy projects done in haskell
Everything in here is published under the BSD3 License

## PXMParser

is a parser for images of the type `*.pbm`, `*.pgm` and `*.ppm` written with
attoparsec, it is a first effort in using it and I am quite happy with that

- DONE: Parsing images with magic value 'P5' and 'P6' is not yet implemented
- TODO: Clean up messy parsing of word16 values
- TODO: Write tests

## ConnectedComponent

an older challenge from amazon code ninjas, a friend told me:

if you have an array consisting of '1's and '0'es find the number ilands of '1's

    1000000000
    0000110000
    0001001000
    0010000100
    0010000100
    0001001000
    0000110000
    0000001000
    1110000111
    0010000101
    1010000101

cells are connected vertically, horizontally and diagonally so the above image
has 4 connected components.

- DONE: List version
- TODO: Map version
- TODO: Array version

## Codingame

implementations of the examples found at codingame.com

    Temperatures ................................. ✔
    ASCII Art .................................... ✔
    Chuck Norris ................................. ✔
    MIME Type .................................... ✔
    Defibrillators ...............................
    Horse-racing Duals ...........................
    ------------------------------------------------
    Stock Exchange Losses ........................
    Network Cabling ..............................
    Conway Sequence ..............................
    Telephone Numbers ............................
    Dwarfs standing on the shoulders of giants ...
    Bender, a depressed robot ....................
    Scrabble ..................................... ✔
    Docteur Who - The Gift .......................
    ------------------------------------------------
    Super Computer ...............................
    Roller Coaster ...............................
    CGX Formatter ................................
    TAN Network ..................................
    Genome Sequencing ............................
    Surface ......................................
    Bender, the money machine ....................
    Bender complexity ............................
    Snakes and ladders ...........................
    The Resistance ...............................
    Doctor Who - Music Sheets ....................
