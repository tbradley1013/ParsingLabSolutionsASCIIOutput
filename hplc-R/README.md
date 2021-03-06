
# Parsing LabSolutions ASCII Output with R

This README shows a basic usage of `parse_hplc` function provided in the
`R/parse-hplc.R` file in this repository. This function allows users to
pass a file path to an output file to the function and it will retrieve
the sample data from the .txt file.

Here I will show a bried example of how it is done.

First, I will `source` the required script. **NOTE:** This *may* move to
a package later depending on want/need from people in the Lab for easier
access/use

``` r
source("parse-hplc.R")
```

This script will take care of installing any required packages that you
do not already have installed on your system. It will also load the
`parse_hplc` function into your global environment. Now we can simply
call the function by giving it a path to an text file that was output
from the LabSolutions HPLC. Here, the `ExampleInput.txt` will be used.

If we do not specify an output path, than by default a tibble will be
returned that can be used for further analysis.

``` r
parse_hplc("example-inputs/ExampleInput.txt")
```

    ## Warning: Calling `as_tibble()` on a vector is discouraged, because the behavior is likely to change in the future. Use `tibble::enframe(name = NULL)` instead.
    ## This warning is displayed once per session.

    ## # A tibble: 131 x 7
    ##    sample_name     sample_id `Ca2+` `K+`    `Mg2+` `Na+`  `NH4+ -N`
    ##    <chr>           <chr>     <chr>  <chr>   <chr>  <chr>  <chr>    
    ##  1 singleinjection 1         0.000  0.000   0.000  0.000  0.000    
    ##  2 singleinjection 2         0.000  0.000   0.000  0.000  0.000    
    ##  3 singleinjection 3         0.000  0.000   0.000  0.000  0.000    
    ##  4 Cation Std Set  1         0.000  0.000   0.000  0.000  0.000    
    ##  5 Cation Std Set  2         0.000  0.626   0.000  0.103  0.156    
    ##  6 Cation Std Set  3         1.250  12.512  1.251  2.056  3.125    
    ##  7 Cation Std Set  4         2.500  24.999  2.503  4.124  6.233    
    ##  8 Cation Std Set  5         4.977  49.924  4.996  8.209  12.482   
    ##  9 Cation Std Set  6         10.005 99.902  10.019 15.972 24.933   
    ## 10 Cation Std Set  7         19.997 200.244 20.043 32.410 50.041   
    ## # ... with 121 more rows

``` r
# # A tibble: 131 x 7
#    sample_name     sample_id `Ca2+` `K+`    `Mg2+` `Na+`  `NH4+ -N`
#    <chr>           <chr>     <chr>  <chr>   <chr>  <chr>  <chr>    
#  1 singleinjection 1         0.000  0.000   0.000  0.000  0.000    
#  2 singleinjection 2         0.000  0.000   0.000  0.000  0.000    
#  3 singleinjection 3         0.000  0.000   0.000  0.000  0.000    
#  4 Cation Std Set  1         0.000  0.000   0.000  0.000  0.000    
#  5 Cation Std Set  2         0.000  0.626   0.000  0.103  0.156    
#  6 Cation Std Set  3         1.250  12.512  1.251  2.056  3.125    
#  7 Cation Std Set  4         2.500  24.999  2.503  4.124  6.233    
#  8 Cation Std Set  5         4.977  49.924  4.996  8.209  12.482   
#  9 Cation Std Set  6         10.005 99.902  10.019 15.972 24.933   
# 10 Cation Std Set  7         19.997 200.244 20.043 32.410 50.041   
# # ... with 121 more rows
```

By default, only the data is returned from this function with one row
representing one sample with all of the analyses given as columns. If
you are returning the info to your R session (i.e. leaving `path = NULL`
as it is by default) than you can also get the rest of the information
for each sample returned by setting `output_type = "full"`.

The raw output of this function can be a bit unwiedly, and it likely
won’t save well to any standard output type, other than maybe json,
but you can explore it via R and RStudio’s View pane. Here is a screen
shot of the output of this function:

![](www/full-report-output-tree.png)

Finally, If you would like to simply save the file to a csv for use in
excel, than you can just provide a path to the desired output.
