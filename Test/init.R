year <- 20
datadir <- file.path('..', 'data')
qb <- file.path(datadir, sprintf('proj_qb%s.csv', year))
rb <- file.path(datadir, sprintf('proj_rb%s.csv', year))
wr <- file.path(datadir, sprintf('proj_wr%s.csv', year))
te <- file.path(datadir, sprintf('proj_te%s.csv', year))
k <- file.path(datadir, sprintf('proj_k%s.csv', year))
cur <- file.path(datadir, sprintf('nfl_current%s.csv', year))

if(file.access(qb) == -1) {
  link <- sprintf('https://github.com/couthcommander/football-values/raw/master/20%s', year)
  download.file(sprintf('%s/proj_qb%s.csv', link, year), qb)
  download.file(sprintf('%s/proj_rb%s.csv', link, year), rb)
  download.file(sprintf('%s/proj_wr%s.csv', link, year), wr)
  download.file(sprintf('%s/proj_te%s.csv', link, year), te)
  download.file(sprintf('%s/proj_k%s.csv', link, year), k)
  download.file(sprintf('%s/nfl_current%s.csv', link, year), cur)
}
