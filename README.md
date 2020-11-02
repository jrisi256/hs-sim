# Understanding the Hearthstone economy

## Introduction

The motivating question concerning this project was how generous (or not) is the Hearthstone economy. How many packs would it take to complete a collection? How many packs would it take to build every viable deck in the meta? How many packs would it take to build that deck you want? How long would it take to do all of these things (given the rate of gold accumulation)? Has the Hearthstone economy been getting less or more generous over time?

This document is meant to be a high-level overview of how the simulation works as it is the simulation of opening packs and generating gold which produces all the estimates I base my analysis on. This is meant as much as a reference for future me as it is for other people who may attempt to use this. I strive to properly document all my code as well as properly testing it and following best programming practices. Nevertheless it is easy to lose the big picture of the program which is what I'll be documenting here.

## The execution flow

* **RunSimulation**: This function initiates the running of the simulation. Importantly you would specify here using the **nrRuns** argument how many times you want the simulation to run. The simulation being a pack-opening simulation. We capture how many packs it would take to complete the *targeted* collection. By default the targeted collection is every card in the set, and by default the simulation will capture how many packs it takes to complete every set ever released in Hearthstone.
    * **PacksToCompletion**: This function is in charge of opening packs until the set is complete. The first function call is made to the function factory **CreateCollection**.
        * **CreateCollection**: This is a function factory (a function which returns another function). When called, it initializes an empty collection of the specified set. It will return a function which can be used to add cards to the collection.
        * Once the function is created for adding cards to our collection, we can start opening packs.
        * **OpenPack**: 

