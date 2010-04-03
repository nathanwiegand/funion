FUNION
======

About
-----
FUNION (**F**use **Union**) presents a unioned view of two or more directories.
As an example:
    /A/fileA1
      /fileA2
      /dir1
        /fileA3
    
    /B/fileB1
      /dir1
        /fileB2
        /dir2/fileB3
    
    
    /FUNION(A,B)/fileA1
                /fileA2
                /fileB1
                /dir1/fileA3
                     /fileB2
                     /dir2/fileB3
    
Common use case is when you have media spread across non-RAIDed disks and you
would like to have a single view of the media.  For example: `/disk1/dvds/{tv,
movies}` and `/disk2/dvds/{tv,movies}`.  You'd prefer to just have
`/dvds/{tv,movies}` which is a unioned view of the two.

Features
--------
Currently supports unioning of 2+ directory trees.  Though, currently the
unioned file-system is read only.


Requirements
------------
* Linix/Unix.
* [FUSE](http://fuse.sourceforge.net/)
* [GHC](http://hackage.haskell.org/platform/) (Haskell platform recommended)

Installation
------------
Install `libfuse-dev`.  On Ubuntu 9.10:
    sudo apt-get install libfuse-dev

Install Funion:
    cabal install funion

Usage
-----
There are currently very few options:
    funion MOUNTPOINT +DIR1 +DIR2 ...
where MOUNTPOINT, DIR1, DIR2, ..., are paths.

