# datetime coercions are symmetric and unchanging

    Code
      print(mat)
    Output
                           date                   datetime<local>        datetime<US/Central>   POSIXlt<US/Central>    duration<mins>   duration<secs>  
      date                 "date"                 "datetime<local>"      "datetime<US/Central>" "datetime<US/Central>" NA               NA              
      datetime<local>      "datetime<local>"      "datetime<local>"      "datetime<US/Central>" "datetime<US/Central>" NA               NA              
      datetime<US/Central> "datetime<US/Central>" "datetime<US/Central>" "datetime<US/Central>" "datetime<US/Central>" NA               NA              
      POSIXlt<US/Central>  "datetime<US/Central>" "datetime<US/Central>" "datetime<US/Central>" "datetime<US/Central>" NA               NA              
      duration<mins>       NA                     NA                     NA                     NA                     "duration<mins>" "duration<secs>"
      duration<secs>       NA                     NA                     NA                     NA                     "duration<secs>" "duration<secs>"

