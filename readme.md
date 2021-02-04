# Manche 0.10A Beta
Copyright 2021, Daniel England.  All Rights Reserved.

## Introduction
Welcome to the Manche readme!  

Manche is a front-end for the Peppito MOD player and allows you to play (or "replay" in the original parlance) MOD files on your MEGA65.

At the moment, Manche and Peppito are in early development stages.  It is intended on making Manche a full MED (MOD EDitor) or "Trakker" and to add more MOD type and effect type support to Peppito as well as various other enhancements.

## License
Manche and Peppito are released under the terms of the LGPL.

## Requirements
Manche and Peppito rely on a very recent (03JAN2021 or later) bitstream in order to play properly.

Manche currently supports NTSC playback adjustments and provides alternate mixing controls for the Nexys based machines. 


## Display
Presently, there is very little to see on the Manche screen.

At the very top is shown the current tick values for Manche and Peppito (including row and sequence counters).

At the very bottom, the current mixer levels are shown.

The middle area is blank until you load a MOD.  This area is divided into two main columns, with the instrument information on the right and MOD and playback information on the left.

## Controls
To load a MOD file, press F1 and enter the name of the file to load.  Press Return to accept the file name and load the MOD file.  Mod files are loaded from the SD card.

To play or stop the loaded MOD file, press Space.

To browse the instruments in the MOD file, use F7 and F8.

To control the master volume, use F9 and F10.

To control the volume of the standard left/right outputs (or just right on the Nexys) use F11/F12.

To control the mix level of the alternate left/rights outputs for changing the "stereo width" (or just the left channel level on the Nexys) use F13/F14. 

## Limitations
The MOD files must be no larger than 256kB (262144 bytes).

The MOD files must be the original M.K. or M!K! types.

The MOD files must not have instruments with sample lengths greater than 65535 bytes long.

## Peppito Performance
At present, Peppito only supports the following effects:
  - Pattern Break
  - Set Volume
  - Set Speed
  - Volume Slide
  - Portamento Up
  - Portamento Down

Notably absent is the instrument Fine Tune control which may cause some issues.  This will be fixed in the near future.  

Also, only the fine speed adjustment is supported, not coarse "Tempo" adjustments.  It is unlikely that this will be supported until after other more important enhancements have been made since it requires very specific playback handling.

## Future Development
It is vital that more testing be done on Manche and Peppito.

If you can find MODs that don't play well but you think they should, I would love to hear from you.  Also, if you specifically know that certain effect types are being used and not supported in the MODs you like to play, please contact me.

## Contact
For further information or to discuss issues with Manche, please contact me at the following address:

	mewpokemon {you know} hotmail {and here} com

Please include the title "Manche" in the subject line or your e-mail might get lost.