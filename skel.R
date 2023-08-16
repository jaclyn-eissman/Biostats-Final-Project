# importProjection
# @files, vector of file names
# return: data.frame
# This function reads and combines projection data into a single data set.  A position column should be added.
# Sort the data by the `fpts` column in descending order.
# ...
# Remove players with duplicate names.  In these cases keep the player with the highest value in the `fpts` column.
importProjection <- function(files) {
    data.frame(PlayerName=NA, Team=NA, position=c('qb','rb','wr','te','k'), pass_yds=100, rush_yds=100, fpts=0)
}

# scaleByGamesPlayed
# @x, data.frame
# @def.gp, numeric, default number of games played
# @alt.gp, named numeric vector, number of games played for teams that haven't played def.gp
# return: data.frame
# This function scales the numeric columns of x by `number of games played / 16`.
# ...
# The NFL season is 17 weeks long, and 11 weeks have been completed.
# Each team plays 16 games and has one week off, called the bye week.
# Most teams have already had their bye week, which means they've played 10 games.
# Teams without a bye week have played 11 games.
# The stats for a player on a team with 10 games played would be scaled by 10/16.
scaleByGamesPlayed <- function(x, def.gp, alt.gp) {
    x
}

# importObserved
# @file, file name
# return: data.frame
# This function reads the observed data.
importObserved <- function(file) {
    data.frame(PlayerName=NA, Team=NA, Pos=c('qb','rb','wr','te','k'), pass_yds=100, rush_yds=100)
}

# topPicks
# @x, data.frame
# @pos, named numeric vector, the number of players to keep per position
# return: data.frame; using the default `pos` would return a data set with 160 rows
# This function should subset the data set by keeping the top players at each position.
# ...
# Define `top player` by ordering the data set by the `fpts` column descendingly.
# If k=20, that means only keep 20 rows where position=='k'.
topPicks <- function(x, pos=c(k=20, qb=20, rb=40, wr=60, te=20)) {
    x
}

# mergeDat
# @x, data.frame, projection data
# @y, data.frame, observed data
# return: data.frame
# This function should merge the projected data with the observed data by the player's name.
# ...
# Keep all rows from the projection data. If observed data is missing, set it to zero.
mergeDat <- function(x, y) {
    x
}

# residuals
# @x, data.frame
# return: list, contains matrix or data.frame for each position
# This function computes the difference between the observed data and the projected data, then splits the data by position.
# ...
# There are 15 columns of interest to compare.  See `lookup` in `test_task1.R` for a description of the columns.
# The place-holder code found in this function should be completely removed, however it gives an approximation of the output.
# Specifically note the data.frame `empty`, as it contains the proper names of the 15 columns each matrix should contain.
residuals <- function(x) {
    pred.pos <- c(qb=20, rb=40, wr=60, te=20, k=20)
    noise <- list()
    empty <- data.frame(fg=0, fga=0, xpt=0, pass_att=0, pass_cmp=0, pass_yds=0, pass_tds=0, pass_ints=0,
                        rush_att=0, rush_yds=0, rush_tds=0, fumbles=0, rec_att=0, rec_yds=0, rec_tds=0)
    for(i in names(pred.pos)) {
        noise[[i]] <- empty[rep(1,pred.pos[i]),]
        row.names(noise[[i]]) <- NULL
    }
    set.seed(35)
    noise$k$fg <- rnorm(20, 0, 3)
    noise$k$fga <- rnorm(20, 0, 4)
    noise$k$xpt <- rnorm(20, 0, 5)
    noise$qb$pass_att <- rnorm(20, 0, 25)
    noise$qb$pass_cmp <- rnorm(20, 0, 15)
    noise$qb$pass_yds <- rnorm(20, 0, 150)
    noise$qb$pass_tds <- rnorm(20, 0, 4)
    noise$qb$pass_ints <- rnorm(20, 0, 2)
    noise$rb$rush_att <- rnorm(40, 0, 40)
    noise$rb$rush_yds <- rnorm(40, 0, 200)
    noise$rb$rush_tds <- rnorm(40, 0, 2)
    noise$rb$fumbles <- rnorm(40, 0, 1)
    noise$wr$rec_att <- rnorm(60, 0, 10)
    noise$wr$rec_yds <- rnorm(60, 0, 160)
    noise$wr$rec_tds <- rnorm(60, 0, 2)
    noise$te$rec_att <- rnorm(20, 0, 10)
    noise$te$rec_yds <- rnorm(20, 0, 120)
    noise$te$rec_tds <- rnorm(20, 0, 2)
    noise
}

# calcPoints
# @l, list, league object
# return: list
# This function calculates fantasy points based on league attributes.
# ...
# Create a new column `points` inside `l$stats`.
# Multiply columns in `l$stats` by respective value in `points`, and sum
calcPoints <- function(l) {
    l$stats[,'points'] <- 0
    l
}

# buildValues
# @l, list, league object
# return: list
# This function calculates dollar values based on league attributes.
# ...
# Create a new column `value` inside `l$stats`.
# Order `l$stats` by `value` descendingly.
# As an example if a league has ten teams and requires one kicker, the tenth best kicker should be worth $1.
# All kickers with points less than the 10th kicker should be removed from `l$stats`.
buildValues <- function(l) {
    l$stats[,'value'] <- 0
    l
}

# league
# @stats, data.frame
# @nteam, numeric, number of teams
# @cap, numeric, money available for each team
# @pos, named numeric vector, position requirements
# @points, named numeric vector, point values
# return: list, league object
# This function creates an object of type `league`.
# ...
# All arguments should remain attributes of the object.
# They define the league setup and will be needed to calculate points and dollar values.
# Call `calcPoints` and `buildValues` to add points and dollar values.
league <- function(stats, nteams, cap, pos, points) {
    l <- list()
    attributes(l) <- list(nteams=nteams, cap=cap, pos=pos, points=points)
    class(l) <- 'league'
    l$stats <- stats
    l <- calcPoints(l)
    l <- buildValues(l)
    l
}

# print.league
# @l, list, league object
# @..., optional additional arguments
# return: league object, invisibly
# This function prints that `stats` element of the league object.
# ...
# At a minimum print these columns: PlayerName,Team,position,points,value.
print.league <- function(l, ...) {
    invisible(l)
}

# plot.league
# @l, list, league object
# @..., optional additional arguments
# return: plot object
# This function should create a scatterplot of dollar values.
# ...
# Add minimal plotting decorations, such as axis labels.
plot.league <- function(l, ...) {
}

# boxplot.league
# @l, list, league object
# @..., optional additional arguments
# return: plot object
# This function should create a boxplot of dollar values by position.
# ...
# Add minimal plotting decorations.
boxplot.league <- function(l, ...) {
}

# hist.league
# @l, list, league object
# @..., optional additional arguments
# return: plot object
# This function should create a histogram of dollar values.
# ...
# Add minimal plotting decorations.
hist.league <- function(l, ...) {
}

# addNoise
# @stats, data.frame
# @noise, list of matrix residuals by position
# return: data.frame
# This function simulates new stats by adding error.
# ...
# As an example assume you want to simulate new projections for quarterbacks.
# The residuals for quarterbacks is a 20x15 matrix.
# Each row from this matrix is no longer identified with a particular player, but rather it's potential error.
# Given the original projection for the first quarterback, sample a row number from the matrix (1 to 20).
# Add the 15 columns from the sampled row to the 15 columns for the first quarterback.
# Repeat the process for every quarterback (as well as every position).
# Most football stats can't be negative so replace any negative values with 0.
addNoise <- function(stats, noise) {
    stats
}

# simleague
# @n, numeric, number of simulations
# @noise, list of matrix residuals by position
# @stats, data.frame
# @nteam, numeric, number of teams
# @cap, numeric, money available for each team
# @pos, named numeric vector, position requirements
# @points, named numeric vector, point values
# return: list, simleague object
# This function creates an object of type `simleague`.
# ...
# A `simleague` is an extension of the `league` object.
# It should contain a new element, `sim`, which is a data.frame.
# It should call `addNoise` `n` times, creating a new projection data set.
# For each simulation, a new league object should be created and its dollar values stored.
# `sim` should have a column for PlayerName and position as well as `n` columns for dollar values.
# `sim` should have the same number of rows as the argument `stats`.
# If a player has no value for a particular simulation, his dollar value should be saved as $0.
simleague <- function(n, noise, stats, nteams, cap, pos, points) {
    l <- league(stats, nteams, cap, pos, points)
    l$sim <- list()
    l
}

# quantile.simleague
# @l, list, simleague object
# @probs, numeric, probability for desired percentile
# return: matrix
# This function calls `quantile` on the simulated dollar values for each player.
# ...
# The number of rows should equal `nrow(l$sim)`.
# The number of columns should equal `length(probs)`.
# Rownames should be set to PlayerName.
quantile.simleague <- function(l, probs=c(0.25, 0.50, 0.75)) {
}

# ci
# This function is complete and should not be modified.
ci <- function(object, ...) {
    UseMethod("ci", object)
}

# ci.default
# This function is complete and should not be modified.
ci.default <- function(object, ...) {
}

# ci.simleague
# @l, list, simleague object
# @probs, numeric, probability for desired percentile
# return: list, ci.simleague object
# This function calculates the top players based on league settings and simulated dollar values.
# ...
# A `ci.simleague` object contains a list element for each position.
# Should call `quantile.simleague`.
# If `probs` has length > 1, use the highest to determine dollar value.
# Sort dollar values in descending order.
# Each list element is a matrix.
# For a given position, the number or rows should equal the number of required players at that position.
# For a given position, the number of columns should equal `length(probs)`.
# For a given position, the rownames should be set to PlayerName.
ci.simleague <- function(l, probs=c(0.25, 0.50, 0.75)) {
}

# print.ci.simleague
# @l, list, ci.simleague object
# @..., optional additional arguments
# return: ci.simleague object, invisibly
# This function converts a list to a printable data.frame.
# ...
# At a minimum print these columns: PlayerName,position,value (for each percentile).
print.ci.simleague <- function(l, ...) {
    invisible(l)
}

# plot.ci.simleague
# @l, list, ci.simleague object
# @pos, character, position
# @..., optional additional arguments
# return: plot object
# This function should create a plot of dollar values for a position.
# ...
# Each percentile defined in the `ci.simleague` object should be plotted on a line.
# Add minimal plotting decorations.
plot.ci.simleague <- function(l, pos='qb', ...) {
}
