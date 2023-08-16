context("Task 2: Creating a League Object")

source('init.R')

stats <- importProjection(c(qb=qb,rb=rb,wr=wr,te=te,k=k))
nteams <- 10
cap <- 200
pos <- c(qb=1, rb=2, wr=3, te=1, k=1)
points <- c(fg=4, xpt=1, pass_yds=1/25, pass_tds=4, pass_ints=-2,
             rush_yds=1/10, rush_tds=6, fumbles=-2, rec_yds=1/20, rec_tds=6)
l <- league(stats, nteams, cap, pos, points)

test_that("league object has correct attributes", {
    expect_true(all(c('nteams','cap','pos','points') %in% names(attributes(l))))
    expect_is(l, 'league')
    expect_equal(nrow(l$stats), sum(pos)*nteams)
    expect_equal(sum(l$stats[,'value']), cap*nteams)
})

test_that("league object has correct values", {
    ix <- grep('Russell Wilson', l$stats[,'PlayerName'])
    expect_equal(ix, 23)
    expect_equal(floor(l$stats[ix,'points']), 304)
    expect_equal(floor(l$stats[ix,'value']), 32)
    ix <- grep('Jonathan Taylor', l$stats[,'PlayerName'])
    expect_equal(floor(l$stats[ix,'points']), 141)
    expect_equal(floor(l$stats[ix,'value']), 1)
    ix <- grep('Austin Hooper', l$stats[,'PlayerName'])
    expect_equal(length(ix), 0)
})

test_that("league handles different settings", {
    testpos <- c(qb=2, rb=2, wr=4, te=1, k=1)
    l <- league(stats, nteams, cap, pos=testpos, points)
    expect_equal(sum(l$stats[,'position'] == 'qb'), testpos[['qb']]*nteams)
    expect_equal(sum(l$stats[,'position'] == 'rb'), testpos[['rb']]*nteams)
    expect_equal(sum(l$stats[,'position'] == 'wr'), testpos[['wr']]*nteams)
    expect_equal(sum(l$stats[,'position'] == 'te'), testpos[['te']]*nteams)
    expect_equal(sum(l$stats[,'position'] == 'k'), testpos[['k']]*nteams)
    expect_equal(sum(l$stats[,'value']), cap*nteams)
    ix <- grep('Terry McLaurin', l$stats[,'PlayerName'])
    expect_equal(floor(l$stats[ix,'value']), 11)
})

test_that("league handles different sizes", {
    l <- league(stats, nteams=14, cap=250, pos, points)
    expect_equal(sum(l$stats[,'position'] == 'qb'), pos[['qb']]*14)
    expect_equal(sum(l$stats[,'position'] == 'rb'), pos[['rb']]*14)
    expect_equal(sum(l$stats[,'position'] == 'wr'), pos[['wr']]*14)
    expect_equal(sum(l$stats[,'position'] == 'te'), pos[['te']]*14)
    expect_equal(sum(l$stats[,'position'] == 'k'), pos[['k']]*14)
    expect_equal(sum(l$stats[,'value']), 250*14)
    ix <- grep('Devin Singletary', l$stats[,'PlayerName'])
    expect_equal(floor(l$stats[ix,'value']), 13)
})

test_that("league handles zeroed position", {
    l <- league(stats, nteams, cap, pos=c(qb=1, rb=2, wr=3, te=1, k=0), points)
    expect_equal(sum(l$stats[l$stats[,'position'] == 'k', 'value']), 0)
})

test_that("league handles missing position", {
    l <- league(stats, nteams, cap, pos=c(qb=1, rb=2, wr=3, te=1), points)
    expect_equal(sum(l$stats[l$stats[,'position'] == 'k', 'value']), 0)
})

test_that("league works without fumbles", {
    l <- league(stats, nteams, cap, pos, points=c(fg=4, xpt=1, pass_yds=1/25, pass_tds=4, pass_ints=-2,
             rush_yds=1/10, rush_tds=6, rec_yds=1/20, rec_tds=6))
    ix <- grep('Dak Prescott', l$stats[,'PlayerName'])
    expect_equal(floor(l$stats[ix,'points']), 319)
})

test_that("league generates error if not enough players", {
    expect_error(league(stats, nteams=100, cap, pos, points))
})

test_that("league generates error when cap is too small", {
    expect_error(league(stats, nteams, cap=4, pos, points))
})

test_that("league generates warning for illegal position", {
    expect_warning(league(stats, nteams, cap, pos=c(qb=1, rb=2, wr=3, te=1, k=1, cb=1), points))
})

test_that("league generates warning for illegal stat category", {
    expect_warning(league(stats, nteams, cap, pos, points=c(fg=4, xpt=1, pass_yds=1/25, pass_tds=4, pass_ints=-2,
             rush_yds=1/10, rush_tds=6, fumbles=-2, rec_yds=1/20, rec_tds=6, spike=1)))
})

test_that("league generates error with bad input", {
    expect_error(league(stats[FALSE,], nteams, cap, pos, points))
    expect_error(league(stats, 0, cap, pos, points))
    expect_error(league(stats, nteams, -1, pos, points))
    expect_error(league(stats, nteams, cap, c(), points))
    expect_error(league(stats, nteams, cap, pos, c()))
})
