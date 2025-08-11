# ToMakeAnAdversaryInadmissible
A collection of software for automating the process of deciding which bets to place on any given night on NBA games. Bets can be placed for over under, spread, and money line. Exhaustive decision rules are emailed to user. 

MasterScript.R is called each morning via a scheduled "TaskScheduler" batch routine, and should be used as a starting point for users interested in learning more about how the code works.

Output of the automated pipeline is shown in the excel files - given Vegas lines, it provides example decision rules for which bets to place that would be sent out daily.

Variable importance plots (along with eponymous R script for making them) is provided for each of the 5 response being modelled. Permutation variable importance as implemented in the MachineShop package is provided. Features ending in '.y' refer to away-team statistics, Features ending in '.x' refer to home-team statistics. 

See https://matthewlouisdavisbiostat.github.io/ for more details and background. 
