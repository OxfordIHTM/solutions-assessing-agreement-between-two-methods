# Statistical methods for assessing agreement between two methods of clinical measurement

<!-- badges: start -->
![License for code](https://img.shields.io/badge/license_for_code-GPL3.0-blue)
![License for text](https://img.shields.io/badge/license_for_writing-CC_BY_4.0-blue)
<!-- badges: end -->


This is a solutions repository for the exercise on **Statistical methods for assessing agreement between two methods of clinical measurement** in the [Open and Reproducible Science in R](https://oxford-ihtm.io/teaching) module of the [MSc in International Health and Tropical Medicine](https://www.tropicalmedicine.ox.ac.uk/study-with-us/msc-ihtm).

<br/>

## Instructions for the assignment
The following tasks have been setup to help students get familiar with functional programming in R.

The students are expected to go through the tasks and appropriately write R code/script to fulfill the tasks and/or to answer the question/s being asked within the tasks. R code/script should be written inside a single R file named `ba.R` and saved in the project’s root directory.

This exercise is based on:

Bland, J. M. & Altman, DouglasG. Statistical Methods For Assessing Agreement Between Two Methods Of Clinical Measurement. Lancet 327, 307–310 (1986).

The dataset used in the paper can be accessed from the teaching_datasets repository. The URL to the .dat file is https://raw.githubusercontent.com/OxfordIHTM/teaching_datasets/main/ba.dat.

The ba dataset contains peak expiratory flow rate (PEFR) measurements (in litres per minute) taken with a Wright peak flow metre (Wright) and a Mini-Wright peak flow metre (Mini-Wright). This is the same data that is presented in the referenced Lancet article above.

<br/>

## Task 1: Read the dataset

Read the `ba.dat` dataset from the GitHub URL provided above by creating a function that will download the dataset from the given URL and then read that dataset into R.

<br/>

## Task 2: Calculate the metrics needed for a Bland and Altman plot

Create a function that will calculate the following metrics required by the Bland and Altman plot as described in the article referenced above:

* Mean of the per subject measurements made by the Wright and the Mini-Wright;

* Difference between the per subject measurements made by the Wright and the Mini-Wright;

* Mean of the difference between the per subject measurements made by the Wright and the Mini-Wright; and,

* Lower and upper limits of agreement between the per subject measurements made by the Wright and the Mini-Wright.

<br/>

## Task 3: Create a Bland and Altman plot

Develop a function that creates a Bland and Altman plot as described in the article.

<br/>

## Solutions

A solutions script named [`functtions.R`](https://github.com/OxfordIHTM/solutions-assessing-agreement-between-two-methods/blob/main/functions.R) is found in this repository. The script provides various possible solutions for each of the tasks above.

A solutions script named [`ba.R`](https://github.com/OxfordIHTM/solutions-assessing-agreement-between-two-methods/blob/ba.R) is found in this repository. The script provides an example R script workflow that utilises the functions created in `functions.R` to perform the required analysis and plotting.

The solutions can also be viewed as an HTML document that shows the code solutions alongside text that provides step-by-step explanation of what the solution is doing. The HTML document can be read [here](http://oxford-ihtm.io/solutions-assessing-agreement-between-two-methods/).

<br/>

## License

All of the written material is made available under the Creative
Commons - Attribution - NonCommercial 4.0 International license (CC-BY-NC-4.0),
while any code is made available under the GNU General Public License Version 3.0 license (GPL-3.0).