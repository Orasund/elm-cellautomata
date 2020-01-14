# Elm-CellAutomata
Elm-CellAutomata is a packages that lets you write your own celluar automatas.

**To get started, head over to CellAutomata.LifeLike**

If you never heard of a celuar automata (or Conways's Game of Life),  
checkout this neat little [simulator](https://ncase.me/simulating/model/). This should give you an idea what this package is about.

When to use it:
* To generate dynamic levels in your game.
* For generative art.

When not to use it:
* A.I. behaviour. It is possible, but you will most likely get very frustrated
* Your rules depend on other things beside the state of the neighbors
* The new state of the cells can not be evaluate indepentend of one another.

## What is a Cellular Automata?
The most famous cellular automata is [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life).
A cell automata takes the state of a cell as well as the state of its neighbors
and then calculate the new state of that cell.

## Where are Cellular Automatas used?
whenever something is dynamically generated in a game, it most often has an underlying cell automata.
There are a few nice automatas for building [landscapes](https://mewo2.com/notes/terrain/) or [dungeons](http://www.roguebasin.com/index.php?title=Cellular_Automata_Method_for_Generating_Random_Cave-Like_Levels).

## Motivation
This package was created along side with a little game of mine (that is not yet finished).
I needed some general abstractions for all my terrain-generation code.
I could not find any package that had something in that direction,
so i created my own.

## Upcoming features
* The `RuleExpression` is missing a `Either (List state)` case
* More modules for different types of fields: **3D Grid**, **triangulations** and **graphs**
* A adaption of [Nicky Case's emoji simulator](https://ncase.me/simulating/model/).
In particular it should be able to generate functional elm code.

## Changelog

* **2.0.0** - Changed the `Order` from `Maybe state -> Int` to `Maybe state -> comparable`.