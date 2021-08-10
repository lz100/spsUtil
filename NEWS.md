# spsUtil 0.1.2

## Major change

-   `quiet` now can mute `message` and `warning` in addition to `print`, `cat`.
-   Rewrite `checkNameSpace` so it does not actually load the package namespace files. This has reduced RAM usage a lot.

## Minor change

-   `msg`'s `.other_color` argument default has changed to `NULL`. When it is `NULL`, use no color decoration.

-   `remove_ANSI` now works on a vector of characters.

# spsUtil 0.1

## Major change

-   migrated from systemPipeShiny
