# projects

<details>

* Version: 2.1.1
* Source code: https://github.com/cran/projects
* URL: https://cran.r-project.org/package=projects
* Date/Publication: 2020-05-29 12:40:02 UTC
* Number of recursive dependencies: 47

Run `cloud_details(, "projects")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    4    42 Art Department          Springfield College  321 University Boulevard, …
    > 
    > # View authors table joined to affiliations table
    > # Notice that multiple rows are created for each affiliation-author combination
    > authors(affiliations = TRUE)
    # A tibble: 6 x 11
         id last_name given_names title degree email phone affiliation_id
      <int> <chr>     <chr>       <chr> <chr>  <chr> <chr>          <int>
    1     1 Rice      Condoleezza <NA>  <NA>   <NA>  <NA>               1
    2     1 Rice      Condoleezza <NA>  <NA>   <NA>  <NA>              42
    3     1 Rice      Condoleezza <NA>  <NA>   <NA>  <NA>               2
    4     1 Rice      Condoleezza <NA>  <NA>   <NA>  <NA>               3
    5    13 Agnew     Spiro       <NA>  LLB    <NA>  <NA>              42
    6   303 <NA>      Plato       <NA>  <NA>   <NA>  <NA>              NA
    # … with 3 more variables: department_name <chr>, institution_name <chr>,
    #   address <chr>
    > 
    > # View only active projects with "Fun" in their title.
    > projects("Fun")
    Error: C stack usage  7969220 is too close to the limit
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      None.# A tibble: 1 x 6
           id title            stage         status  deadline_type deadline           
        <int> <chr>            <prjstg>      <chr>   <chr>         <dttm>             
      1     1 Understanding t… 4: manuscript waitin… submission    2055-02-28 00:00:00
      # A tibble: 2 x 7
        author_id last_name given_names title degree email         phone       
            <int> <chr>     <chr>       <chr> <chr>  <chr>         <chr>       
      1        13 Agnew     Spiro       <NA>  LLB    <NA>          <NA>        
      2      8888 Stone     Rosetta     <NA>  PhD    slab@rock.net 867-555-5309
      # A tibble: 1 x 3
        current_owner corresp_auth creator 
        <prjaut>      <prjaut>     <prjaut>
      1 13: Agnew     8888: Stone  0: root 
      Error: C stack usage  7973652 is too close to the limit
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

