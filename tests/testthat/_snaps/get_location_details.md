# basic case works

    Code
      get_location_details(list(x = longitudes, y = latitudes))
    Message
      Passing 5 coordinates to the ArcGIS single coordinate geocoder
    Output
      # A tibble: 5 x 8
          lat    lng region           region_abbr subregion city  neighborhood address
        <dbl>  <dbl> <chr>            <chr>       <chr>     <chr> <chr>        <chr>  
      1  49.2 -124.  British Columbia BC          Nanaimo ~ Nana~ <NA>         <NA>   
      2  53.9 -123.  British Columbia BC          Fraser-F~ Prin~ West Bowl    3861 S~
      3  43.7  -79.4 Ontario          ON          Toronto ~ Toro~ <NA>         100 Qu~
      4  49.1 -123.  British Columbia BC          Greater ~ Surr~ West Newton  6245 1~
      5  56.0 -122.  British Columbia BC          Peace Ri~ Huds~ <NA>         Cadenh~

