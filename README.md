# Elm-CellAutomata
Elm-CellAutomata is a packages that lets you write your own celluar automatas.

If you never heard of a celuar automata (or Conways's Game of Life),  
checkout this neat little [simulator](https://ncase.me/simulating/model/).  
This should give you an idea what this package is about.

## What is a Cellular Automata?
The most famous cellular automata is [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life).
A cell automata takes the state of a cell as well as the state of its neighbors
and then calculate the new state of that cell.

## Where are Cellular Automatas used?
Thenever something is dynamically generated in a game, it most often has an underlying cell automata.
There are a few nice automatas for building landscapes or dungeons,
but in this documentation we will mostly focus on famous mathematical automatas.
(Because they are easy and get the point across)

## Motivation
This package was created along side with a little game of mine (that is not yet finished.).
I needed some generall abstractions for all my terrain-generation code.
The elm community had a few projects in that sort but not to the scale that i would need it,
so i created this package.

## Upcoming Features
* More modules for different types of fields: **3D Grid**, **triangulations** and **graphs**
* A adaption of [Nicky Case's emoji simulator](https://ncase.me/simulating/model/).
In particular it should be able to generate functional elm code.