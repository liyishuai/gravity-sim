N-particle simulation
===========
Arman Tokanov (arman), Yishuai Li (yishuai)
##

This is a fork of [gravity-sim](https://github.com/BartoszMilewski/gravity-sim),
a simple many-body gravity simulation written in Haskell. The simulator uses a
Yesod web server, JSON, and HTML 5 to display the animated positions of bodies
in a browser.

Dependencies: yesod, repa

Overview of files:
 * **Types.hs:** all types used in the project
 * **Physics.hs:** encoding of physical laws used by the simulation
 * **Simulation.hs:** simulation logic
 * **World.hs:** initial configurations of all simulated worlds
 * **Main.hs:** the main module that runs a simulation
 * **Test.hs:** tests