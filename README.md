# NFL Referee Analysis

A Shiny App (deploying soon) that provides users with the ability to analyze referee penalty tendencies in multiple different ways.

### Data:
This Shiny App sources data from nflfastR from 1999 until the current NFL season.

### Interpretation:
The goal of this Shiny App is to understand the general tendencies of all referees, in addition to how specific referees have called certain penalties, as well as how certain game situation affect their tendencies. nflfastR provides penalty data on each play since 1999, and nflreadr has the crew chief for each game since 1999. Using the data nflfastR and nflreadr provide, a user can find out and identify the tendencies for a referee with a variety of different filtration options, such as referee, time frame, penalties, and various game situation variables. This Shiny App can be extremely valuable in game planning for a specific crew chief, to identify prior penalty calling tendencies over their career, how they compare to other referees, and how they may have reffed in prior, recent games.

### How to use this Shiny App:
The tabs in this Shiny App are:
- Leaderboards creates a list of all the referees that fit the selected inputs.
- Reports creates an individual report for any selected referee for the selected inputs.
- Game Comparison provides an individual game comparison for a referee in any selected game, and compares their penalty frequency to their average over the course of that season and their referee career.
- Referee Lookup provides information on the games referee'd for a selected referee.

### Possible Limitations:
While this Shiny App offers a way to evaluate referee tendencies, there are a variety of factors to take into account, in addition to ways to improve the analysis. For example, the data doesn't include information for which referee made which calls, it only provides the crew chief for that game. For certain games or seasons referee'd, it's likely that the frequency of penalties are a byproduct of the actions of the teams involved (whether that includes a higher or lower frequency of penalties). Additionally, just because a referee has a certain proclivity to (or not to) make a certain call, that doesn't mean it's the more likely case.
