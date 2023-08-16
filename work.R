library(ggplot2)
library(reshape2)

# importProjection
# @files, vector of file names
# return: data.frame
# This function reads and combines projection data into a single data set.  A position column should be added.
# Sort the data by the `fpts` column in descending order.
# ...
# Remove players with duplicate names.  In these cases keep the player with the highest value in the `fpts` column.
importProjection <- function(files) {
    p <- names(files)
    dat <- lapply(files, read.csv)
    cname <- unique(unlist(lapply(dat, names)))
    for(i in seq_along(dat)) {
        dat[[i]][,setdiff(cname, names(dat[[i]]))] <- 0
        dat[[i]] <- dat[[i]][,cname]
        dat[[i]][,"position"] <- p[i]
    }
    df <- do.call(rbind, dat)
    df <- df[order(df[,"fpts"], decreasing=TRUE),]
    df[!duplicated(df[,"PlayerName"]),]
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
    data <- data.frame()
    y <- split(x,x$Team)
    for(i in names(y)) {
        if(i %in% names(alt.gp)) {
            for(j in y[i]) {
                cols <- sapply(j, is.numeric)
                j[cols] <- lapply(j[cols],function(s) s*alt.gp[[i]]/16)
                k <- cbind(j[!cols], j[cols])
            }
        } else {
            for(j in y[i]) {
                cols <- sapply(j, is.numeric)
                j[cols] <- lapply(j[cols],function(s) s*def.gp/16)
                k <- cbind(j[!cols], j[cols])
            }
        }
        data <- rbind(data,k)
        data <- data[order(match(data$PlayerName, x$PlayerName)),]
    }
    data
}


# importObserved
# @file, file name
# return: data.frame
# This function reads the observed data.
importObserved <- function(file) {
    read.csv(file)
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
    x_order <- x[order(x[,"fpts"], decreasing=TRUE),]
    top <- data.frame()
    for(i in names(pos)) {
        pos_only <- x_order[x_order$position==i,]
        top_pos <- pos_only[1:pos[[i]],]
        top <- rbind(top,top_pos)
    }
    top
}


# mergeDat
# @x, data.frame, projection data
# @y, data.frame, observed data
# return: data.frame
# This function should merge the projected data with the observed data by the player's name.
# ...
# Keep all rows from the projection data. If observed data is missing, set it to zero.
mergeDat <- function(x, y) {
    names(x)[1] <- "PlayerName"
    names(y)[1] <- "PlayerName"
    merged <- merge(x,y,by="PlayerName",all.x=TRUE)
    merged[is.na(merged)] = 0
    merged
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
    projected_col=c('fg','fga','xpt','pass_att','pass_cmp','pass_yds','pass_tds','pass_ints',
                    'rush_att','rush_yds','rush_tds','rec_att','rec_yds','rec_tds','fumbles')
    observed_col=c("FGM","FGA","XPM","Att.pass","Cmp.pass","Yds.pass","TD.pass","Int.pass",
                   "Att.rush","Yds.rush","TD.rush","Rec.catch","Yds.catch","TD.catch","Fmb")
    r <-  x[,observed_col] - x[,projected_col]
    names(r) <- projected_col
    position <- x$position
    r_pos <- cbind(r, position)
    r_pos <- split(r_pos, r_pos$position)
    r_pos <- lapply(r_pos, function(x) { x["position"] <- NULL; x })
    r_pos
}


# calcPoints
# @l, list, league object
# return: list
# This function calculates fantasy points based on league attributes.
# ...
# Create a new column `points` inside `l$stats`.
# Multiply columns in `l$stats` by respective value in `points`, and sum
calcPoints <- function(l) {
    points <- attr(l,"points")
    #point calculations and give warning if illegal stats category
    for(i in names(points)) {
        if(!(i %in% c("fg","xpt","pass_yds","pass_tds","pass_ints","rush_yds",
                      "rush_tds","fumbles","rec_yds","rec_tds"))) {
            warning("illegal stat category")
        } else {
            l$stats[,sprintf("pts_%s", i)] <- l$stats[,i]*points[i]
        }
    }
    l$stats[,"points"] <- rowSums(l$stats[grepl("^pts", names(l$stats))])
    l$stats[grepl("^pts", names(l$stats))] <- NULL
    l$stats
}


#buildValues
# @l, list, league object
# return: list
# This function calculates dollar values based on league attributes.
# ...
# Create a new column `value` inside `l$stats`.
# Order `l$stats` by `value` descendingly.
# As an example if a league has ten teams and requires one kicker, the tenth best kicker should be worth $1.
# All kickers with points less than the 10th kicker should be removed from `l$stats`.
buildValues <- function(l) {
    pos <- attr(l,"pos")
    nteams <- attr(l,"nteams")
    cap <- attr(l,"cap")
    #check if enough teams 
    if(nteams<=0){
        stop("not enough teams")
    }
    #check if positions vector is missing
    if(length(pos)==0){
        stop("no positions present")
    }
    #check if enough players
    req <- vector()
    for(a in names(pos)){
        req[a] <- pos[a]*nteams
        total <- sum(req)
        if(total>nrow(l$stats)){
            stop("not enough players")
        }
    }
    #account for a missing position 
    for(a in c("qb","rb","te","wr","k")) {
        if(!(a %in% names(pos))) {
            pos[a] <- 0
        }
    }
    #values calculation 
    l$stats <- l$stats[order(l$stats[,"points"], decreasing=TRUE),]
    for(i in names(pos)) {
        if(!(i %in% c("qb","rb","te","wr","k"))) {
            warning("illegal position")
        } else {
            ix <- which(l$stats[,"position"] == i)
            baseline <- pos[i]*nteams
            if(baseline == 0) {
                l$stats[ix, "marg"] <- -1
            } else {
                l$stats[ix, "marg"] <- l$stats[ix,"points"] - l$stats[ix[baseline],"points"]
            }
        }
    }
    l$stats <- l$stats[l$stats[,"marg"] >= 0,]
    l$stats[,"value"] <- l$stats[,"marg"]*(nteams*cap-nrow(l$stats))/sum(l$stats[,"marg"]) + 1
    l$stats <- l$stats[order(l$stats[,"value"], decreasing=TRUE),]
    #check if cap is too small
    for(i in l$stats[,"value"]){
        if(i < 1){
            stop("cap is too small")
        }
    }
    l$stats
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
    class(l) <- "league"
    l$stats <- stats
    l$stats <- calcPoints(l)
    l$stats <- buildValues(l)
    l
}


# print.league
# @l, list, league object
# @..., optional additional arguments
# return: league object, invisibly
# This function prints that `stats` element of the league object.
# ...
# At a minimum print these columns: PlayerName,Team,position,points,value.
print.league <- function(l,...) {
    stats <- l$stats[,c("PlayerName","Team","position","points","value")]
    print(stats)
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
    df <- l$stats
    df <- df[order(df[,"value"], decreasing=TRUE),]
    df$rank <- seq(1:nrow(df))
    ggplot(data=df, aes(x=rank, y=value)) + geom_point() + xlab("Ranking") + ylab("Dollar Value")
}


# boxplot.league
# @l, list, league object
# @..., optional additional arguments
# return: plot object
# This function should create a boxplot of dollar values by position.
# ...
# Add minimal plotting decorations.
boxplot.league <- function(l, ...) {
    df <- l$stats
    ggplot(data=df, aes(x=position, y=value)) + geom_boxplot() + xlab("Position") + ylab("Dollar Value")
}


# hist.league
# @l, list, league object
# @..., optional additional arguments
# return: plot object
# This function should create a histogram of dollar values.
# ...
# Add minimal plotting decorations.
hist.league <- function(l, ...) {
    df <- l$stats
    ggplot(df, aes(x=value)) + geom_histogram(binwidth=10,color="black",fill="white") +
        ggtitle("League Histogram") + xlab("Dollar Value") + ylab("Frequency")
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
    x <- stats
    noise <- lapply(noise, as.matrix)
    new_stats <- list()
    for(a in unique(stats$position)) {
        res <- noise[[a]]
        stats_pos <- stats[stats$position==a,]
        cols <- intersect(names(stats_pos), colnames(res))
        for(i in 1:nrow(stats_pos)) {
            error <- res[sample(nrow(res), 1, replace=TRUE),cols]
            stats_pos[i,][cols] <- error + stats_pos[i,][cols]
        }
        new_stats[[a]] <- stats_pos 
    }
    out <- Reduce(function(...) rbind(...), new_stats)
    out[out < 0] <- 0
    out <- out[order(match(out$PlayerName, x$PlayerName)),]
    out
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
    x <- stats$PlayerName
    l <- league(stats, nteams, cap, pos, points)
    cols <- data.frame(stats$PlayerName,stats$position)
    names(cols) <- c("PlayerName","position")
    new_proj <- list()
    new_proj <- replicate(n, addNoise(stats, noise), simplify=F)
    l_new_proj <- lapply(new_proj, function(x) league(x, nteams, cap, pos, points))
    stats <- lapply(l_new_proj, function(x) x$stats)
    sim <- lapply(stats, function(x) x[,c("PlayerName","value")])
    sim <- lapply(sim, function(x) merge(x,cols,by="PlayerName",all.y=T))
    for(i in 1:length(sim)){
        colnames(sim[[i]]) <- c("PlayerName",i,"position")
    }
    sim <- Reduce(function(...) merge(..., by=c("PlayerName","position")), sim)
    sim <- sim[order(match(sim$PlayerName, x)),]
    sim[is.na(sim)] <- 0
    l$sim <- sim
    class(l) <- "simleague"
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
    x <- l$sim[,"PlayerName"]
    sim_stats <- l$sim
    row.names(sim_stats) <- sim_stats$PlayerName
    sim_stats$PlayerName <- NULL
    sim_stats$position <- NULL
    sim_stats <- as.matrix(sim_stats)
    quants <- t(apply(sim_stats, 1, FUN = function(x) quantile(x, c(1, probs))))[,-1,drop=FALSE]
    quants <- quants[order(match(row.names(quants), x)),,drop=FALSE]
    quants
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
    pos <- attr(l,"pos")
    nteams <- attr(l,"nteams")
    x <- l$sim[,"PlayerName"]
    info <- l$sim[,c("PlayerName","position")]
    qts <- quantile.simleague(l, probs)
    qts <- as.data.frame(qts)
    qts$PlayerName <- row.names(qts)
    row.names(qts) <- NULL
    qts <- merge(info,qts,by="PlayerName")
    max <- paste0(max(probs)*100,"%")
    qts <- qts[order(qts[,max], decreasing=TRUE),]
    top <- data.frame()
    for(i in names(pos)) {
        pos_only <-  qts[qts$position==i,]
        top_pos <- pos_only[1:(nteams*pos[[i]]),]
        top <- rbind(top,top_pos)
    }
    row.names(top) <- top$PlayerName
    top$PlayerName <- NULL
    top_qts_pos <- split(top, top$position)
    top_qts_pos <- lapply(top_qts_pos, function(x) { x["position"] <- NULL; x })
    class(top_qts_pos) <- "ci.simleague"
    top_qts_pos
}


# print.ci.simleague
# @l, list, ci.simleague object
# @..., optional additional arguments
# return: ci.simleague object, invisibly
# This function converts a list to a printable data.frame.
# ...
# At a minimum print these columns: PlayerName,position,value (for each percentile).
print.ci.simleague <- function(l, ...) {
    l.pos <- Map("[<-", l, "position", value = names(l))
    df.l <- Reduce(function(...) rbind(...), l.pos)
    df.l$PlayerName <- row.names(df.l)
    row.names(df.l) <- NULL
    df.l <- df.l[,c(which(colnames(df.l)=="position"),which(colnames(df.l)!="position"))]
    df.l <- df.l[,c(which(colnames(df.l)=="PlayerName"),which(colnames(df.l)!="PlayerName"))]
    print(df.l)
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
plot.ci.simleague <- function(l, pos="qb", ...) {
    #check position
    if(!(pos %in% c("qb","rb","te","wr","k"))) {
        stop("illegal position")
    } else {
        l.pos <- Map("[<-", l, "position", value = names(l))
        df.l <- Reduce(function(...) rbind(...), l.pos)
        df.l.pos <- df.l[df.l$position==pos,]
        df.l.pos$position <- NULL
        df.l.pos$rank <- seq(1:nrow(df.l.pos))
        df.l.pos <- melt(df.l.pos, id.var="rank")
        ggplot(df.l.pos, aes(rank, value, linetype=variable)) + geom_line() + xlab("Ranking") + ylab("Dollar Value") +
            guides(linetype=guide_legend(title="Percentiles"))
    }
}



