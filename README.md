# xwjoy-redux
Throttle Support for DOS *X-Wing* / *TIE Fighter* games

## About
*xwjoy-redux* is an updated version of a utility from the '90s for LucasArts' *X-Wing* and *TIE Fighter* games (DOS versions, 1993-1995) that added joystick throttle support.  I am not the original creator of that utility, and don't have the original source code.  But, since that utility was so small, it was relatively easy to recreate the original binary with new source files and add new features.

I assume that original utility is freeware.  Nobody gave me any permission to use, modify, reverse-engineer it.

#### What functionality does xwjoy 2.0 offer?
*xwjoy.com* currently has two significant improvements over the original *joy.com*.
- **Continuous throttle**: Throttle adjustments are in ~3% increments (the native unit within the game), instead of the 33% increments in the original.
- **Persistent calibration**: Throttle calibration data is stored on disk and loaded from there.  (The original *joy.com* required calibration every time it was executed.)

## Installing
#### Overview
Simply copy *xwjoy.com* to the disk and execute xwjoy.com (execute once and only once per boot cycle) before running *X-Wing* or *TIE Fighter*.  On the first run, it will prompt you to move the throttle to its limits and press a joystick button at each limit.

#### Installing in DOSBox (GOG version, possibly similar on Steam?)
1. Copy *xwjoy.com* to the game subdirectory.
  * Linux: */home/user/.local/share/STAR WARS Tie Fighter Special Edition/data*
  * Windows: *C:\GOG Games\Star Wars - TIE Fighter CD (1995)*
2. In the GOG DOS CD-ROM version of *TIE Fighter*, there's a file *dosbox_tiecd_single.conf*.  Before the line `tie.exe`, add a new line `xwjoy.com`.

#### Recalibrating
Calibration data is stored on disk in a file called *xwjoy.cal*.  If you want to recalibrate your joystick, delete this file and run *xwjoy* again.  Be sure to reboot if you run *xwjoy* multiple times per boot cycle, before playing again.

## Building
If you'd like to build it, requirements are:
- **DOS-compatible environment**: MS-DOS, DOSBox, etc.
- **MASM 6.11**: Other MASM versions untested, may work.  Install into the *devenv/MASM611* directory, if you want to be compatible with the included *dosbox-devenv.conf*.

There's an *nmake*-compatible makefile in the *devenv/xwjoy* directory.  Running that will assemble the source.

