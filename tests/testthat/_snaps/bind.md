# vec_rbind() name repair messages are useful

    Code
      vec_rbind(1, 2)
    Message <simpleMessage>
      New names:
      * `` -> ...1
      New names:
      * `` -> ...1
    Output
        ...1
      1    1
      2    2
    Code
      vec_rbind(1, 2, .names_to = NULL)
    Message <simpleMessage>
      New names:
      * `` -> ...1
      New names:
      * `` -> ...1
    Output
        ...1
      1    1
      2    2
    Code
      vec_rbind(1, 2, ...10 = 3)
    Message <simpleMessage>
      New names:
      * `` -> ...1
      New names:
      * `` -> ...1
      New names:
      * `` -> ...1
    Output
        ...1
      1    1
      2    2
      3    3
    Code
      vec_rbind(1, 2, ...10 = 3, .names_to = NULL)
    Message <simpleMessage>
      New names:
      * `` -> ...1
      New names:
      * `` -> ...1
      New names:
      * `` -> ...1
    Output
           ...1
      ...1    1
      ...2    2
      ...3    3
    Code
      vec_rbind(a = 1, b = 2)
    Message <simpleMessage>
      New names:
      * `` -> ...1
      New names:
      * `` -> ...1
    Output
        ...1
      1    1
      2    2
    Code
      vec_rbind(a = 1, b = 2, .names_to = NULL)
    Message <simpleMessage>
      New names:
      * `` -> ...1
      New names:
      * `` -> ...1
    Output
        ...1
      a    1
      b    2
    Code
      vec_rbind(c(a = 1), c(b = 2))
    Output
         a  b
      1  1 NA
      2 NA  2
    Code
      vec_rbind(c(a = 1), c(b = 2), .names_to = NULL)
    Output
         a  b
      1  1 NA
      2 NA  2

# vec_rbind() is silent when assigning duplicate row names of df-cols

    Code
      vec_rbind(df, df)
    Output
         mpg
      1 21.0
      2 21.0
      3 22.8
      4 21.0
      5 21.0
      6 22.8

---

    Code
      vec_rbind(mtcars[1:4, ], mtcars[1:3, ])
    Output
                         mpg cyl disp  hp drat    wt  qsec vs am gear carb
      Mazda RX4...1     21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
      Mazda RX4 Wag...2 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
      Datsun 710...3    22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
      Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
      Mazda RX4...5     21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
      Mazda RX4 Wag...6 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
      Datsun 710...7    22.8   4  108  93 3.85 2.320 18.61  1  1    4    1

# vec_cbind() name repair messages are useful

    Code
      vec_cbind(1, 2)
    Message <simpleMessage>
      New names:
      * `` -> ...1
      * `` -> ...2
    Output
        ...1 ...2
      1    1    2
    Code
      vec_cbind(1, 2, ...10 = 3)
    Message <simpleMessage>
      New names:
      * `` -> ...1
      * `` -> ...2
      * ...10 -> ...3
    Output
        ...1 ...2 ...3
      1    1    2    3
    Code
      vec_cbind(a = 1, b = 2)
    Output
        a b
      1 1 2
    Code
      vec_cbind(c(a = 1), c(b = 2))
    Message <simpleMessage>
      New names:
      * `` -> ...1
      * `` -> ...2
    Output
        ...1 ...2
      1    1    2

