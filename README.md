# AutomateStudio
Automate Studio is an IDE for the scripting Language AutoIt3.

## How to Build
This IDE is written in ObjectPascal using the Lazarus IDE. It is currently only buildable using Lazarus 1.6.4, as since than a lot of changes has been conducted which are currently incompatible with the current version of Lazarus. In Lazarus 1.6.4 just open the Project at `IDE/AU3IDE.lpi` and build it via `Run->Build` or debug it via the Lazarus debugger interface

In order to function properly, the IDE expects some configuration files, which has to be provided in the directory of the executeable. These files can either be found in the Release package of the most current version here at Github, or can be downloaded by running the Updater.exe in that directory. For doing the latter just compile the updater at `IDE/Updater/Updater.lpi` and execute it. Lazarus will automatically compile it into the correct directory, so simply running the Updater project from within Lazarus should be enough
