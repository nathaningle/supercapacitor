# supercapacitor

supercapacitor generates [XSPF](http://www.xspf.org/) playlists for entire albums on demand.  The goal is to be able to point [VLC](https://www.videolan.org/) at a URL served by supercapacitor and have it play an entire album, given a collection of music files arranged in subdirectories by artist and album.

Its [namesake](https://en.wikipedia.org/wiki/Supercapacitor) also delivers excess pF.

## Building

### With cabal

First, ensure your `cabal-install` version is at least 1.24.  Then, from the top level of this repository:

    cabal new-configure
    cabal new-build

### With stack

From the top level of this repository:

    stack init
    stack setup
    stack build

## Music library setup

supercapacitor expects your music files to be arranged thusly:

1. A top-level directory, containing:
2. Directories named for the recording artists, containing:
3. Directories named for albums, containing:
4. The music files.

## Running it

The `supercap` executable takes two arguments:

1. The filesystem path to the music library's top-level directory.
2. The root URL that `supercap` will be serving files at.  For testing this might be http://localhost:8000/.
