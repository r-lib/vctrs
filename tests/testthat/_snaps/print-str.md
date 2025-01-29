# show attributes

    Code
      obj_str(x)
    Output
       int [1:100]  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16...
       @ x: chr "a string"
       @ y: int [1:20] 1 2 3 4 5 6 7 8 9 10 ...
       @ z:'data.frame':	3 obs. of  1 variable:
        ..$ x: int [1:3] 1 2 3

---

    Code
      obj_str(mtcars)
    Output
      df[,11] [1:32] 
      'data.frame':	32 obs. of  11 variables:
      $ mpg : num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
      $ cyl : num  6 6 4 6 8 6 8 4 4 6 ...
      $ disp: num  160 160 108 258 360 ...
      $ hp  : num  110 110 93 110 175 105 245 62 95 123 ...
      $ drat: num  3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
      $ wt  : num  2.62 2.88 2.32 3.21 3.44 ...
      $ qsec: num  16.5 17 18.6 19.4 17 ...
      $ vs  : num  0 0 1 1 0 1 0 1 1 1 ...
      $ am  : num  1 1 1 0 0 0 0 0 0 0 ...
      $ gear: num  4 4 4 3 3 3 3 4 4 4 ...
      $ carb: num  4 4 1 1 2 1 4 2 2 4 ...
      @ row.names: chr [1:32] "Mazda RX4" "Mazda RX4 Wag" "Datsun 710" "Hornet 4 Drive" ...

# max argument (#1355)

    Code
      x <- vctrs::new_vctr(letters)
      print(x, max = 5)
    Output
      <vctrs_vctr[26]>
      [1] a b c d e
      ... and 21 more
      Set `max` to a larger value to show all items.
    Code
      print(x, max = 30)
    Output
      <vctrs_vctr[26]>
       [1] a b c d e f g h i j k l m n o p q r s t u v w x y z

# small max.print option (#1355)

    Code
      x <- vctrs::new_vctr(letters)
      print(x)
    Output
      <vctrs_vctr[26]>
      [1] a b c d e
      ... and 21 more
      Set `options(max.print = )` to a larger value to show all items.

# large max.print option (#1355)

    Code
      x <- vctrs::new_vctr(letters)
      print(x)
    Output
      <vctrs_vctr[26]>
       [1] a b c d e f g h i j k l m n o p q r s t u v w x y z

# both max argument and max.print option (#1355)

    Code
      x <- vctrs::new_vctr(letters)
      print(x, max = 5)
    Output
      <vctrs_vctr[26]>
      [1] a b c d e
      ... and 21 more
      Set `max` to a larger value to show all items.
    Code
      print(x, max = 20)
    Output
      <vctrs_vctr[26]>
       [1] a b c d e f g h i j k l m n o p q r s t
      ... and 6 more
      Set `options(max.print = )` to a larger value to show all items.
    Code
      print(x, max = 30)
    Output
      <vctrs_vctr[26]>
       [1] a b c d e f g h i j k l m n o p q r s t u v w x y z

