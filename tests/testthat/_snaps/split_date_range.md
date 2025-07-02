# basic case works

    Code
      split_date_range(date_range, max_duration = "3 days")
    Output
             start        end
      1 2020-01-01 2020-01-04
      2 2020-01-04 2020-01-07
      3 2020-01-07 2020-01-10
      4 2020-01-10 2020-01-12

---

    Code
      split_date_range(date_range, max_duration = "3 days", as_list = TRUE)
    Output
      <list_of<
        tbl_df<
          start: date
          end  : date
        >
      >[4]>
      [[1]]
      # A tibble: 1 x 2
        start      end       
        <date>     <date>    
      1 2020-01-01 2020-01-04
      
      [[2]]
      # A tibble: 1 x 2
        start      end       
        <date>     <date>    
      1 2020-01-04 2020-01-07
      
      [[3]]
      # A tibble: 1 x 2
        start      end       
        <date>     <date>    
      1 2020-01-07 2020-01-10
      
      [[4]]
      # A tibble: 1 x 2
        start      end       
        <date>     <date>    
      1 2020-01-10 2020-01-12
      

