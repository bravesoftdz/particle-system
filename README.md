particle-system
===============

Simulates gravity and electromagnetism

It works till 1000 particles (no compiler optimizations)

To do:
- Improve user interface
- Still working on grid-optimizing system.
- Add reactions between particles. 
    Probably need to add that to the setupdata.txt as well.
- Ferro-magnetism, dielectric, spin etc.

Future:
- Split up setupdata.txt and put the seperate systems in a directory
- Web-versions of the simulator
- statistics
- Feinmann diagrams
- Speed of light
   (The forces in a cell can be kept for a while with the time-tags.
    If a particle arrives in a cell, this cell can use the time-tag to
    retreive the force at a certain time.)
- Relativity
   (Formulas depend on the speed of the particles.)

Compiling with Lazarus.
http://www.lazarus.freepascal.org/
(works on Windows, Linux, Mac OS X, Android, IOS, +more)

No compiler?
You only need particles.exe and setupdata.txt to work with it.

What it does:
It displays the simulation in a window. 
You can zoom in/out, pan, rotate with the mouse.
pan=drag-drop
zoom=scroll
rotate=right mouse button.  (does not seem to work well in planet system)

How to start:

SETUP

Use setup->generate particles to setup your particle system. 
Select the configuration that is stored in the setupdata.txt
(Checkboxes are not functional yet..)

SIMULATE

Generate random selection if you want to create a large amount of particles.
Keep count below 1000 if you want it to show simulation in real-time.
Use simulate->calculate single step to  autocalc to contious 

I calibrated the program on the solar planetary system (planets)
(Accuracy set to 0.01 and max-step to 0.01 day.)

