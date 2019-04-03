# Port Royal strategy

Port Royal is a 2-5 player card game published by Pegasus Games. While playing, I noticed that some basic strategies seemed to be more successful than others.

This project contains a model of the game and a simulation engine that can be used for (statistically) evaluating the winning chances of different strategies.

This project is NOT a playable (video) game and it's not meant to be one.

Part of the goal of this project was also to find out how well suited non-trivial immutable data structures are for simulations (in terms of performance).

### Performance

A two player game with very fast but stupid agents (which just pick a random action and do no further processing) takes an average of about 1.5ms to simulate and has a length of about 375 moves.

### TODO

- [x] Working, tested model of the game
- [] Refactor some of the ugly parts of the model
- [x] Simple simulation engine
- [] Implement more complex heuristics
- [] Implement more complex move planning
- [] Implement some sort of genetic algorithm for finding promising strategies (e.g., card hiring preferences)
