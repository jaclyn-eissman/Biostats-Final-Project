context("Task 3: Simulating Confidence Intervals")

source('init.R')

stats <- importProjection(c(qb=qb,rb=rb,wr=wr,te=te,k=k))
obs <- importObserved(cur)
n <- 10
gp <- c(CAR=11, TB=11)
scproj <- scaleByGamesPlayed(stats, n, gp)
best <- topPicks(scproj)
mdat <- mergeDat(best, obs)
res <- residuals(mdat)

nteams <- 10
cap <- 200
pos <- c(qb=1, rb=2, wr=3, te=1, k=1)
points <- c(fg=4, xpt=1, pass_yds=1/25, pass_tds=4, pass_ints=-2,
             rush_yds=1/10, rush_tds=6, fumbles=-2, rec_yds=1/20, rec_tds=6)

test_that("addNoise modifies stats with residuals", {
    simstat <- addNoise(stats, res)
    expect_equal(dim(stats), dim(simstat))
    expect_equal(stats[,'PlayerName'], simstat[,'PlayerName'])
    pnoise <- res[[as.character(stats[1,'position'])]]
    cn <- intersect(colnames(pnoise), names(stats))
    expect_true(any(abs(simstat[1,cn] - stats[1,cn]) > 0))
    expect_equal(sum(simstat[,cn] < 0), 0)
    ix <- which(stats[,'position'] == 'qb')[1]
    expect_true(any(abs((simstat[ix,'pass_yds'] - stats[ix,'pass_yds']) - res[['qb']][,'pass_yds']) < 1e-5))
    ix <- which(stats[,'position'] == 'rb')[1]
    expect_true(any(abs((simstat[ix,'rush_yds'] - stats[ix,'rush_yds']) - res[['rb']][,'rush_yds']) < 1e-5))
    ix <- which(stats[,'position'] == 'wr')[1]
    expect_true(any(abs((simstat[ix,'rec_yds'] - stats[ix,'rec_yds']) - res[['wr']][,'rec_yds']) < 1e-5))
    ix <- which(stats[,'position'] == 'te')[1]
    expect_true(any(abs((simstat[ix,'rec_tds'] - stats[ix,'rec_tds']) - res[['te']][,'rec_tds']) < 1e-5))
    ix <- which(stats[,'position'] == 'k')[1]
    expect_true(any(abs((simstat[ix,'xpt'] - stats[ix,'xpt']) - res[['k']][,'xpt']) < 1e-5))
})

test_that("simleague creates sim attribute", {
    l <- simleague(100, res, stats, nteams, cap, pos, points)
    expect_is(l, 'simleague')
    expect_true('sim' %in% names(l))
    expect_equal(dim(l$sim), c(nrow(stats), 102))
    expect_equal(sum(is.na(l$sim)), 0)
    if(length(l$sim) == 0) {
        expect_error(1)
    } else {
        # each sim should have at least 5 players worth $1
        expect_true(all(colSums(l$sim[,-c(1,2)] == 1) >= 5))
    }
})

test_that("quantile applies to simulated leagues", {
    ls <- simleague(100, res, stats, nteams, cap, pos, points)
    if(length(ls$sim) == 0) {
      ls <- numeric(nrow(stats))
    }
    qls <- quantile(ls)
    expect_equal(nrow(qls), nrow(stats))
    expect_equal(colnames(qls), c("25%", "50%", "75%"))
    expect_equal(rownames(qls), stats[,'PlayerName'])
    qls <- quantile(ls, probs=c(0,1))
    expect_equal(colnames(qls), c("0%", "100%"))
    qls <- quantile(ls, probs=c(0.8))
    expect_equal(colnames(qls), "80%")
})

test_that("ci applies to simulated league", {
    ls <- simleague(100, res, stats, nteams, cap, pos, points)
    cls <- ci(ls, probs=c(0.1, 0.9))
    expect_is(cls, 'ci.simleague')
    expect_equal(length(cls), length(pos))
    expect_equal(nrow(cls[['rb']]), unname(pos['rb']*nteams))
    expect_equal(colnames(cls[['te']]), c("10%", "90%"))
    cls <- ci(ls)
    ix <- cls[['wr']][,'75%']
    expect_false(is.null(ix))
    if(is.null(ix)) {
      ix <- numeric(0)
    }
    expect_equal(order(ix, decreasing=TRUE), seq(pos['wr']*nteams))
    if('ci.simleague' %in% class(cls)) {
      expect_error(plot(cls, pos='CB'))
    } else {
      expect_error(1)
    }
})

test_that("top players should appear in reasonable ranks", {
    ls <- simleague(100, res, stats, nteams, cap, pos, points)
    cls <- ci(ls, probs=0.75)
    toprank <- seq(5)
    expect_true('Justin Tucker' %in% rownames(cls[['k']])[toprank])
    expect_true('Patrick Mahomes' %in% rownames(cls[['qb']])[toprank])
    expect_true('Ezekiel Elliott' %in% rownames(cls[['rb']])[toprank])
    expect_true('Davante Adams' %in% rownames(cls[['wr']])[toprank])
    expect_true('George Kittle' %in% rownames(cls[['te']])[toprank])
})
