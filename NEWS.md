# spsUtil 0.2.2

## Minor Change

-   `checkNamespace` now uses `find.package` instead of `.packages` function. The latter is known to be expansive. This improves the speed of this function in large library system.

# spsUtil 0.2.1

## New feature

-   New function `strUniquefy`: takes a string or numeric vector and found all duplicated elements and add an index to the end to make sure all elements are unique.

## Minor Change

-   Use faster `.packages(TRUE)` method in `checkNamespace` to find installed packages.

# spsUtil 0.2.0

## New feature

-   `spsOption` has a new argument `.list`: set more than one options a time by passing a list of options and their values to it.
-   New function `simpleStack`, a simple stack data structure with methods in R6 class. Methods include `pop`, `push` and more.
-   New function `historyStack`, a stack data structure to store history steps in R6 class. Similar to browser, word editor or image editor history, stored data can be moved forward or backward by step(s).
-   New in-line operation functions `inc`, `mult`, `divi`, which are equivalent of `1 += 1`, `1 *= 2`, `i /= 2` in other programming languages.
-   New function `timeout`, run expressions with time limits.
-   `checkNameSpace` has new arguments, `time_out` and `on_timeout`. This handles if the installed package list to large and waiting time is too long. Instead of waiting, when times out, directly return all checking packages as missing, but can be customize to other behavior as well.

## Minor Change

-   `emptyIsFalse` now named `notFalsy` and its opposite is `isFalsy`, old name `emptyIsFalse` still reminds in the package.

## Bug fix

-   Fix NA bugs in `emptyIsFalse`.

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
