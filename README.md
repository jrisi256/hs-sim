# Understanding the code - High level overview

## Introduction

The motivating question concerning this project was how generous (or not) is the Hearthstone economy. How many packs would it take to complete a collection? How many packs would it take to build every viable deck in the meta? How many packs would it take to build that deck you want? How long would it take to do all of these things (given the rate of gold accumulation)? How much do you have to spend to achieve the aforementioned goals? Has the Hearthstone economy been getting less or more generous over time?

This document is meant to be a high-level overview of how the simulation works as it is the simulation of opening packs and generating gold which produces all the estimates I base my analysis on. This is meant as much as a reference for future me as it is for other people who may attempt to use this. I strive to properly document all my code as well as properly testing it and following best programming practices. Nevertheless it is easy to lose the big picture of the program which is what I'll be documenting here.

## RunSimulation

This function initiates the running of the simulation. Importantly you would specify here using the **nrRuns** argument how many times you want the simulation to run. The simulation being a pack-opening simulation. We capture how many packs it would take to complete the *targeted* collection. By default the targeted collection is every card in the set, and by default the simulation will capture how many packs it takes to complete every set ever released in Hearthstone.

### PacksToCompletion

This function is in charge of opening packs until the set is complete. The first function call is made to the function factory **CreateCollection**.

#### CreateCollection

This is a function factory (a function which returns another function). When called, it initializes an empty collection of the specified set. It will return a function which can be used to add cards to the collection.

Once the function is created for adding cards to our collection, we can start opening packs.

#### OpenPack

As the name suggests, this function opens a pack. Packs, at this point, are 5 random draws from the set of common, rare, epic, legendary, golden common, golden rare, golden epic, and golden legendary. The distribution of these rarities is estimated using data from pitytracker.com. Any individual pack or streak of pack openings may not be realistic (e.g. you may got a pack of all commons, you may go more than 40 packs without opening a legendary), but I anticipate my pack openings, on average and given large enough numbers, will be equivalent to what would happen if you were to open real Hearthstone packs.

I also incorporate the more generous pity timer when opening packs from a new set (guaranteed legendary in the first 10 packs which is different from the usual 40). I don't have any data concerning the distribution of rarities in people's first 10 pack openings so I tried to be as judicious as possible when designing this section. There's also the issue of the fact that the pitytracker.com data is already incorporating into its estimated distribution of rarities the fact that people draw legendaries at a higher rate in their first 10 packs. Nevertheless I still think it's useful for estimating the *relative* difference in the number of packs which need to be opened if you have the more generous pity timer vs. not having it.

The flow of **OpenPack** is as follows:

* Given the more generous pity timer, sample from the set of no legend, legendary, and golden legendary. By default, the more generous pity timer is turned off. In the **PacksToCompletion** function, a pity timer is provided which increases by 0.1 for each pack that is opened without opening a legendary or golden legendary. By the time the 10th pack is being opened, P(Picking a legendary or golden legendary) = 1. Once a legendary is picked, the pity timer is turned off.
* Given that a legend wasn't picked (no legend, this will be the default case), open a pack like normal (5 draws).
* If a legend or golden legendary was picked, pick 4 cards from the a modified set of common, rare, epic, golden common, golden rare, and golden epic. The probabilities for picking legendaries and golden legendaries are redistributed evenly across the remaining rarities.
* Once the rarity of all 5 cards has been set, we call the function created by **CreateCollection** (generally I call it **AddCardFunc**) 5 times (once for each card in the pack). To be concrete about this, the cards in the pack at this point are just rarities.

##### AddCardFunc

This function adds a card to your collection. The flow is as follows:

* Given the set and the rarity, determine the sample space of cards from which we'll be randomly picking.
  * If duplicate protection is on for cards of all rarities, remove from the sample space all cards in the collection of the current rarity which we already have full copies of. If we have full copies of every card, then randomly sample from all possible cards of the current rarity.
  * Else if duplicate protection is on for legendaries AND the current draw is a legendary, remove from the sample space all cards in the collection which are legendary, and which we already have full copies of. If we have full copies of every legendary card, then randomly sample for all possible legendary cards.
  * Else if pack duplicate protection is on, find all copies of cards (of the current rarity) which have had the maximum number of copies opened already in the pack (2 for everything but legendaries). Remove those cards from the sample space.
* Pick a card form the sample space.
* Add the card to the *phantom collection*. The phantom collection is every card ever opened regardless of if we dusted it or not. Since we automatically dust all golden cards, we need to keep track of these cards for duplicate protection purposes (to mirror the system in Hearthstone which similarly tracks every card ever opened regardless of whether or not it was dusted).
* If we drew a golden card, and we aren't keeping them, automatically dust it.
* Else if we drew a non-target card, and we aren't keeping them, automatically dust it. By default we keep opening packs until we have a complete collection of the entire set. However one can specify a *target collection* which would be some subset of the full collection one would want to collect instead.
* Else check if we have full copies of the current card in our collection. If we do, dust it. If we don't, add it to our collection.
* Add the card opened to our pack.
* Return our current collection, current dust total, and card which was opened.

#### Back to OpenPack

* Return the 5 cards opened in the pack as well as the cumulative dust total.

### Back to PacksToCompletion

* Add the opened pack to our log of packs.
* Given the current state of our collection, have we collected every targeted card (by default the entire set is targeted)? Allow for options to use or to not use dust. Continue opening packs until every target card has been collected. Once the target collection is complete, return the log of packs opened.

## Back to RunSimulation

* Once every target collection has been completed for every set for every run, create a data frame with all the relevant data like the number of packs it took and collection metadata (duplicate protection, dusting golden cards, etc.).
* Create another data frame which keeps track of the amount of dust accumulated with each pack that was opened in addition to all the relevant metadata.
* Return both data frames.