# cs-440-spring-2017
## CS 440 Pokefight Programming Language

### Poke-Fight
### A Pokemon Fighting Card Game Programming Language: Whitepaper
##### By: Alrick Sayasavanh, David Ghiurco, Szymon Krzeptowski-Mucha

#Introduction
The purpose of our language is to allow programmers to manipulate Pokemon types and power level. Programmers will be able to define their own Pokemon as well as they're power levels. The language will allow programmers to be able to test different relations with certain types and create their own rules for their own Pokemon world. Once the types and power levels have been established. The language creates a frame for programming a turn-based battle between two Pokemon to test which one wins.

## Language
Poke-Fight is a simple, portable language for 1-vs-1 simulations from each base type {Grass, Electric, Water, Fire}.

## Simple and Intuitive
The language abstracts the turn based mechanics and simplifies the programming of a fight between various Pokemon. The custom data type Pokemon has two attributes attached to it, which base type and its power. This allows the programmer to add new pokemon to the set easily.

## Portable
One of the primary goals of this programming languages is to be highly portable to allow cross-platform development of Pokemon fight games. As such, we will be using only core libraries beneath the scenes to allow for maximum portability.

## Language Features
### Custom Loops <br />
The game will be turn-based, making turns implemented by loops that continue until a Pokemon has defeated the other.

### Deterministic Battling <br />
The game will be deterministic, meaning the results are based off Pokemon types and power level. If a Pokemon is a type that is effective against another type, the stronger type's power level will enhance and the weaker type's power will be diminished.

### Custom Data Types <br />
There are several data types that are specialized for Pokemon. These values would be the following: Pokemon, Type, and Power. Pokemon provide the Type and Power. Type determines the attribute of the attacks provided by the Pokemon. Power determines how strong the attacks are provided by the Pokemon.

### The Runtime <br />
The core of Poke-Fight is a turn-based text interface that will allow a programmer to create a 1-vs-1 Pokemon tournament program. Once written, the program will declare the winner of the 2 players based on the type and power level of the used Pokemon.


# Grammar
The grammar will specify what inputs are valid in the language <br />
Note: <br />
Terminals are lowercase. Non-Terminals are uppercase. <br />

*Program => Pokefight main ( ) { Statements } <br />
Statements  => { Statement } <br />
Statement   => Assignment | Fight <br />
Assignment  => Declaration = Expression ; <br />
Declaration => Type identifier <br />
Expression  => int <br />
Type        => water | fire | grass | electric <br />
Fight       => identifier fight identifier ; <br />*

This grammar is _unambiguous_ because it contains no operators of different precedence and recursion is kept to a minimum. For any input, there will be only one left-most derivation. <br />

We chose to keep the grammar unambiguous for now to only support 1v1 fights. It may become more complicated in the future if we decide to add many-vs-many pokemon fight functionality.

# How to Run (Command-line Interpreter)

```
$ make clean && make
$ ./pokemon
```
