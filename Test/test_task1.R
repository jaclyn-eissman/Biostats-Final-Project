context("Task 1: Finding Residuals")

source('init.R')

lookup <- data.frame(Name=c('field goals','field goals attempted','extra points','passing attempts','passing completions',
                      'passing yards','passing touchdowns','passing interceptions','rushing attempts','rushing yards',
                      'rushing touchdowns','receiving attempts','receiving yards','receiving touchdowns','fumbles'), 
               projected_col=c('fg','fga','xpt','pass_att','pass_cmp','pass_yds','pass_tds','pass_ints',
                           'rush_att','rush_yds','rush_tds','rec_att','rec_yds','rec_tds','fumbles'),
               observed_col=c("FGM","FGA","XPM","Att.pass","Cmp.pass","Yds.pass","TD.pass","Int.pass",
                              "Att.rush","Yds.rush","TD.rush","Rec.catch","Yds.catch","TD.catch","Fmb"), stringsAsFactors = FALSE)
proj <- importProjection(c(qb=qb,rb=rb,wr=wr,te=te,k=k))
obs <- importObserved(cur)
n <- 10
gp <- c(CAR=11, TB=11)
scproj <- scaleByGamesPlayed(proj, n, gp)
best <- topPicks(scproj)
mdat <- mergeDat(best, obs)
res <- residuals(mdat)

test_that("projection data is imported", {
    expect_true(all(c('position', lookup[,'projected_col']) %in% names(proj)))
    expect_equal(sum(duplicated(proj[,'PlayerName'])), 0)
    expect_equal(dim(proj), c(742, 19))
    expect_equal(sapply(split(proj, proj[,'position']), nrow), c(k=37, qb=100, rb=173, te=143, wr=289))
})

test_that("data is transformed by games played", {
    ngp <- gp[match(proj[,'Team'], names(gp))]
    ngp[is.na(ngp)] <- n
    expect_true(all(abs(scproj[, 'pass_yds'] - proj[, 'pass_yds']*ngp/16) < 1e-4))
    expect_true(all(abs(scproj[, 'rush_yds'] - proj[, 'rush_yds']*ngp/16) < 1e-4))
})

test_that("projection data is reduced to top players", {
    posbest <- c(k=20, qb=20, rb=40, wr=60, te=20)
    expect_equal(nrow(best), sum(posbest))
    bestrb <- best[best[,'position'] == 'rb',]
    expect_equal(order(bestrb[,'fpts'], decreasing=TRUE), seq(40))
    expect_equal(nrow(bestrb), 40)
    expect_equal(sum(best[,'position'] == 'k'), 20)
})

test_that("observed data is imported", {
    expect_true(all(c('Pos', lookup[,'observed_col']) %in% names(obs)))
    # these tests will change with updated data
    expect_equal(dim(obs), c(579, 18))
    expect_equal(sum(obs[,'Pos'] == 'qb', na.rm = TRUE), 54)
    expect_equal(sum(obs[,'Pos'] == 'wr', na.rm = TRUE), 159)
})

test_that("projected and observed data are merged", {
    expect_equal(dim(mdat), c(160, 36))
    expect_equal(anyDuplicated(mdat[,'PlayerName']), 0)
    if('FGA' %in% names(mdat)) {
      expect_equal(sum(is.na(mdat[,'FGA'])), 0)
    } else{
      expect_true('FGA' %in% names(mdat))
    }
})

test_that("category differences produce residuals", {
    expect_true(any(res[['rb']][,'rec_att'] != 0))
    # unlike the rest, these pass by default
    expect_is(res, 'list')
    expect_equal(length(res), 5)
    pos <- c(qb=20, rb=40, wr=60, te=20, k=20)
    expect_true(all(names(pos) %in% names(res)))
    expect_equal(sapply(res, nrow)[names(pos)], pos)
})
