*Due Monday, 7 December, 6:00 PM*

240 points total.

Submit a single R script (named `work.R`) by email to cole.beck@vumc.org.

All work should be done by the student, please no collaboration. You may ask the instructor for help or clarification.

You may use any code found in the [football-values lecture](https://github.com/couthcommander/football-values) or from homework solutions.

This project directory should contain the following:

  * data/
  * ex-figs/
  * tests/
  * instructions.md
  * report.Rmd
  * skel.R

The `data` directory has six CSV files and the `tests` directory has four R scripts. Four example plots are saved in `ex-figs`.

You should not need to modify any of these files, as I will use the same files to evaluate your script.

`skel.R` contains place-holders for the functions you will need to create. You should copy and save this file as `work.R`.

Please ensure that the `knitr` and `testthat` R packages are installed.

The final product is created by knitting `report.Rmd`. Note that this file sources `work.R`.

You may test your work by setting the working directory in R to the project directory. Then run the following:

```
source('work.R'); testthat::test_dir('tests', reporter = 'summary')
```

Use the tests' error messages to add functionality and verify your results. A `.` indicates a passed test. Each task is associated with a test file, that you should view for more infomation.

There are 22 task 1 tests, worth three points each (66 points). There are 67 tests for task 2 and 3, worth two points each (134 points). The report should produce 8 plots (see examples ex-figs/) and print 2 objects - plots are worth three points (24 points) and each print is worth eight (16 points).

The goal of this project is to compare observed values to projected values, and to use the difference (residuals) to simulate new values.

# Example Output

![scatterplot](ex-figs/scatterplot.png)
![boxplot](ex-figs/boxplot.png)
![histogram](ex-figs/histogram.png)
![QB-CI plot](ex-figs/QB-CIplot.png)
