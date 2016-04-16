# Misc_R_scripts

A collection of miscellaneous ad hoc R scripts

**factorially_paste_rows.R** takes a data frame of strings as it's input and returns a vector where each element is a factorial combination of each column. For example:

| col1 | col2 | col3  |
| -----|------| ------|
| one  | red  | apple |
| two  | blue | pear  |

becomes
```R
c("one:red:apple", "one:red:pear", "one:blue:apple", "one:blue:pear", "two:red:apple", "two:red:pear", "two:blue:apple", "two:blue:pear")
```

**factorially_paste_lists.R** does the same thing as above but for lists containing vectors of unequal length. For example:
```R
> list
$col1
[1] "one"
$col2
[1] "red"  "blue"
$col3
[1] "apple" "pear" 
```

becomes
```R
c("one:red:apple", "one:red:pear", "one:blue:apple", "one:blue:pear")
```

**generate_distance_data.R** takes a dist-class object and a data frame with meta-data and outputs a date frame with pairwise distances for each sample and the meta-data comparisons of interest.

**generate_label_df.R** and **generate_label_df_errobars.R** take a ggplot2 boxplot or errobar plot, respectively, the original data used for the plot, and an ANOVA equation and provide significance letter with appropriate x- and y-values to add to the plot. Both functions also return the ANOVA used to generate the significance letters.
