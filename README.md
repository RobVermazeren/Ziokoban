# Ziokoban (NEEDS TO BE UPDATED)

## Introduction

At a 5-day Functional Scala course by John A. De Goes in September 2018, I was introduced to the [ZIO](https://zio.dev) library. Afterwards I really wanted to start playing around with this library. So I started this project.
I have been regularly working on it ever since, trying to keep up was every new release of the library.

This is a playground project, so its main aim is to get to know the ZIO library. I chose to make a game, because I wanted something with garanteed side effects. I chose [Sokoban](https://en.wikipedia.org/wiki/Sokoban), because it is one of my favorite games ever!

## Screenshot

![screenshot](images/screenshot.png)

## Functionality
The current features/restrictions are:
- Played in the terminal.
- Levels are read from a slc file, containing a set of sokobon levels. This file is not included.
  Example source of slc files is [this sokoban website](http://www.sourcecode.se/sokoban/levels)
- Slc file to use is configured in `ziokoban.conf`.  
- Switch to other level:
  - N key: Next level
  - U key: Next unsolved level 
  - P key: Previous level 
- After solving a level, the game will switch to the next unsolved level.  
- Currently results are not persisted, so every time the game is started all levels need to be played.
- Navigation using arrow keys and WASD keys.
- Undo moves with X key.
- Quit the game with the Q key.
- Built and tested on Arch Linux. Test on Windows showed issues with the Unicode characters used. (But these can be changed in the config file.)

## Some ZIO keywords
- ZIO ;-)
- Layers
- Queue
- Ref
- Schedule
- ZIO Config
- ZIO Test
 
## Ideas for future features 
My list of possible improvements and extentions:
- Add more unit tests.
- Add logging. (displaying in separate part of screen?)
- Add a game clock and game info/stats on screen.
- Add dead block detection. (real time parallel calculations and output)
- Use STM somewhere.
- Introduce second kind of GameOutput. (GUI)

## License
This project is licensed under the MIT License. See [LICENSE.txt](LICENSE.txt) fo details.

