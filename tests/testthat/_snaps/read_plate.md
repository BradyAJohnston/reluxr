# Reading tecan plate files.

    Code
      plate_read_tecan(fl)
    Warning <lifecycle_warning_deprecated>
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"signal"` instead of `.data$signal`
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"value"` instead of `.data$value`
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"signal"` instead of `.data$signal`
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"value"` instead of `.data$value`
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"cycle_nr"` instead of `.data$cycle_nr`
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"well"` instead of `.data$well`
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"cycle_nr"` instead of `.data$cycle_nr`
    Output
      # A tibble: 11,520 x 4
          time well   od600  lumi
         <dbl> <chr>  <dbl> <dbl>
       1     1 A01   0.0450   -11
       2     1 A02   0.0452     1
       3     1 A03   0.0453   -11
       4     1 A04   0.0453    -1
       5     1 A05   0.0453     2
       6     1 A06   0.0452     4
       7     1 A07   0.0458   -11
       8     1 A08   0.0456   -11
       9     1 A09   0.0455     4
      10     1 A10   0.0451     1
      # ... with 11,510 more rows

