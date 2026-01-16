# Internal FAQ - Implementation of `vec_locate_matches()`

[`vec_locate_matches()`](https://vctrs.r-lib.org/reference/vec_locate_matches.md)
is similar to
[`vec_match()`](https://vctrs.r-lib.org/reference/vec_match.md), but
detects *all* matches by default, and can match on conditions other than
equality (like `>=` and `<`). There are also various other arguments to
limit or adjust exactly which kinds of matches are returned. Here is an
example:

    x <- c("a", "b", "a", "c", "d")
    y <- c("d", "b", "a", "d", "a", "e")

    # For each value of `x`, find all matches in `y`
    # - The "c" in `x` doesn't have a match, so it gets an NA location by default
    # - The "e" in `y` isn't matched by anything in `x`, so it is dropped by default
    vec_locate_matches(x, y)
    #>   needles haystack
    #> 1       1        3
    #> 2       1        5
    #> 3       2        2
    #> 4       3        3
    #> 5       3        5
    #> 6       4       NA
    #> 7       5        1
    #> 8       5        4

## Algorithm description

### Overview and `==`

The simplest (approximate) way to think about the algorithm that
`df_locate_matches_recurse()` uses is that it sorts both inputs, and
then starts at the midpoint in `needles` and uses a binary search to
find each needle in `haystack`. Since there might be multiple of the
same needle, we find the location of the lower and upper duplicate of
that needle to handle all duplicates of that needle at once. Similarly,
if there are duplicates of a matching `haystack` value, we find the
lower and upper duplicates of the match.

If the condition is `==`, that is pretty much all we have to do. For
each needle, we then record 3 things: the location of the needle, the
location of the lower match in the haystack, and the match size (i.e.
`loc_upper_match - loc_lower_match + 1`). This later gets expanded in
`expand_compact_indices()` into the actual output.

After recording the matches for a single needle, we perform the same
procedure on the LHS and RHS of that needle (remember we started on the
midpoint needle). i.e. from `[1, loc_needle-1]` and
`[loc_needle+1, size_needles]`, again taking the midpoint of those two
ranges, finding their respective needle in the haystack, recording
matches, and continuing on to the next needle. This iteration proceeds
until we run out of needles.

When we have a data frame with multiple columns, we add a layer of
recursion to this. For the first column, we find the locations of the
lower/upper duplicate of the current needle, and we find the locations
of the lower/upper matches in the haystack. If we are on the final
column in the data frame, we record the matches, otherwise we pass this
information on to another call to `df_locate_matches_recurse()`, bumping
the column index and using these refined lower/upper bounds as the
starting bounds for the next column.

I think an example would be useful here, so below I step through this
process for a few iterations:

    # these are sorted already for simplicity
    needles <- data_frame(x = c(1, 1, 2, 2, 2, 3), y = c(1, 2, 3, 4, 5, 3))
    haystack <- data_frame(x = c(1, 1, 2, 2, 3), y = c(2, 3, 4, 4, 1))

    needles
    #>   x y
    #> 1 1 1
    #> 2 1 2
    #> 3 2 3
    #> 4 2 4
    #> 5 2 5
    #> 6 3 3

    haystack
    #>   x y
    #> 1 1 2
    #> 2 1 3
    #> 3 2 4
    #> 4 2 4
    #> 5 3 1

    ## Column 1, iteration 1

    # start at midpoint in needles
    # this corresponds to x==2
    loc_mid_needles <- 3L

    # finding all x==2 values in needles gives us:
    loc_lower_duplicate_needles <- 3L
    loc_upper_duplicate_needles <- 5L

    # finding matches in haystack give us:
    loc_lower_match_haystack <- 3L
    loc_upper_match_haystack <- 4L

    # compute LHS/RHS bounds for next needle
    lhs_loc_lower_bound_needles <- 1L # original lower bound
    lhs_loc_upper_bound_needles <- 2L # lower_duplicate-1

    rhs_loc_lower_bound_needles <- 6L # upper_duplicate+1
    rhs_loc_upper_bound_needles <- 6L # original upper bound

    # We still have a 2nd column to check. So recurse and pass on the current
    # duplicate and match bounds to start the 2nd column with.

    ## Column 2, iteration 1

    # midpoint of [3, 5]
    # value y==4
    loc_mid_needles <- 4L

    loc_lower_duplicate_needles <- 4L
    loc_upper_duplicate_needles <- 4L

    loc_lower_match_haystack <- 3L
    loc_upper_match_haystack <- 4L

    # last column, so record matches
    # - this was location 4 in needles
    # - lower match in haystack is at loc 3
    # - match size is 2

    # Now handle LHS and RHS of needle midpoint
    lhs_loc_lower_bound_needles <- 3L # original lower bound
    lhs_loc_upper_bound_needles <- 3L # lower_duplicate-1

    rhs_loc_lower_bound_needles <- 5L # upper_duplicate+1
    rhs_loc_upper_bound_needles <- 5L # original upper bound

    ## Column 2, iteration 2 (using LHS bounds)

    # midpoint of [3,3]
    # value of y==3
    loc_mid_needles <- 3L

    loc_lower_duplicate_needles <- 3L
    loc_upper_duplicate_needles <- 3L

    # no match! no y==3 in haystack for x==2
    # lower-match will always end up > upper-match in this case
    loc_lower_match_haystack <- 3L
    loc_upper_match_haystack <- 2L

    # no LHS or RHS needle values to do, so we are done here

    ## Column 2, iteration 3 (using RHS bounds)

    # same as above, range of [5,5], value of y==5, which has no match in haystack

    ## Column 1, iteration 2 (LHS of first x needle)

    # Now we are done with the x needles from [3,5], so move on to the LHS and RHS
    # of that. Here we would do the LHS:

    # midpoint of [1,2]
    loc_mid_needles <- 1L

    # ...

    ## Column 1, iteration 3 (RHS of first x needle)

    # midpoint of [6,6]
    loc_mid_needles <- 6L

    # ...

In the real code, rather than comparing the double values of the columns
directly, we replace each column with pseudo "joint ranks" computed
between the i-th column of `needles` and the i-th column of `haystack`.
It is approximately like doing
`vec_rank(vec_c(needles$x, haystack$x), type = "dense")`, then splitting
the resulting ranks back up into their corresponding needle/haystack
columns. This keeps the recursion code simpler, because we only have to
worry about comparing integers.

### Non-equi conditions and containers

At this point we can talk about non-equi conditions like `<` or `>=`.
The general idea is pretty simple, and just builds on the above
algorithm. For example, start with the `x` column from needles/haystack
above:

    needles$x
    #> [1] 1 1 2 2 2 3

    haystack$x
    #> [1] 1 1 2 2 3

If we used a condition of `<=`, then we'd do everything the same as
before:

- Midpoint in needles is location 3, value `x==2`

- Find lower/upper duplicates in needles, giving locations `[3, 5]`

- Find lower/upper *exact* match in haystack, giving locations `[3, 4]`

At this point, we need to "adjust" the `haystack` match bounds to
account for the condition. Since `haystack` is ordered, our "rule" for
`<=` is to keep the lower match location the same, but extend the upper
match location to the upper bound, so we end up with `[3, 5]`. We know
we can extend the upper match location because every haystack value
after the exact match should be less than the needle. Then we just
record the matches and continue on normally.

This approach is really nice, because we only have to exactly match the
`needle` in `haystack`. We don't have to compare each needle against
every value in `haystack`, which would take a massive amount of time.

However, it gets slightly more complex with data frames with multiple
columns. Let's go back to our original `needles` and `haystack` data
frames and apply the condition `<=` to each column. Here is another
worked example, which shows a case where our "rule" falls apart on the
second column.

    needles
    #>   x y
    #> 1 1 1
    #> 2 1 2
    #> 3 2 3
    #> 4 2 4
    #> 5 2 5
    #> 6 3 3

    haystack
    #>   x y
    #> 1 1 2
    #> 2 1 3
    #> 3 2 4
    #> 4 2 4
    #> 5 3 1

    # `condition = c("<=", "<=")`

    ## Column 1, iteration 1

    # x == 2
    loc_mid_needles <- 3L

    loc_lower_duplicate_needles <- 3L
    loc_upper_duplicate_needles <- 5L

    # finding exact matches in haystack give us:
    loc_lower_match_haystack <- 3L
    loc_upper_match_haystack <- 4L

    # because haystack is ordered we know we can expand the upper bound automatically
    # to include everything past the match. i.e. needle of x==2 must be less than
    # the haystack value at loc 5, which we can check by seeing that it is x==3.
    loc_lower_match_haystack <- 3L
    loc_upper_match_haystack <- 5L

    ## Column 2, iteration 1

    # needles range of [3, 5]
    # y == 4
    loc_mid_needles <- 4L

    loc_lower_duplicate_needles <- 4L
    loc_upper_duplicate_needles <- 4L

    # finding exact matches in haystack give us:
    loc_lower_match_haystack <- 3L
    loc_upper_match_haystack <- 4L

    # lets try using our rule, which tells us we should be able to extend the upper
    # bound:
    loc_lower_match_haystack <- 3L
    loc_upper_match_haystack <- 5L

    # but the haystack value of y at location 5 is y==1, which is not less than y==4
    # in the needles! looks like our rule failed us.

If you read through the above example, you'll see that the rule didn't
work here. The problem is that while `haystack` is ordered (by
[`vec_order()`](https://vctrs.r-lib.org/reference/vec_order.md)s
standards), each column isn't ordered *independently* of the others.
Instead, each column is ordered within the "group" created by previous
columns. Concretely, `haystack` here has an ordered `x` column, but if
you look at `haystack$y` by itself, it isn't ordered (because of that 1
at the end). That is what causes the rule to fail.

    haystack
    #>   x y
    #> 1 1 2
    #> 2 1 3
    #> 3 2 4
    #> 4 2 4
    #> 5 3 1

To fix this, we need to create haystack "containers" where the values
within each container are all *totally* ordered. For `haystack` that
would create 2 containers and look like:

    haystack[1:4,]
    #> # A tibble: 4 × 2
    #>       x     y
    #>   <dbl> <dbl>
    #> 1     1     2
    #> 2     1     3
    #> 3     2     4
    #> 4     2     4

    haystack[5,]
    #> # A tibble: 1 × 2
    #>       x     y
    #>   <dbl> <dbl>
    #> 1     3     1

This is essentially what `computing_nesting_container_ids()` does. You
can actually see these ids with the helper,
`compute_nesting_container_info()`:

    haystack2 <- haystack

    # we really pass along the integer ranks, but in this case that is equivalent
    # to converting our double columns to integers
    haystack2$x <- as.integer(haystack2$x)
    haystack2$y <- as.integer(haystack2$y)

    info <- compute_nesting_container_info(haystack2, condition = c("<=", "<="))

    # the ids are in the second slot.
    # container ids break haystack into [1, 4] and [5, 5].
    info[[2]]
    #> [1] 0 0 0 0 1

So the idea is that for each needle, we look in each haystack container
and find all the matches, then we aggregate all of the matches once at
the end. `df_locate_matches_with_containers()` has the job of iterating
over the containers.

Computing totally ordered containers can be expensive, but luckily it
doesn't happen very often in normal usage.

- If there are all `==` conditions, we don't need containers (i.e. any
  equi join)

- If there is only 1 non-equi condition and no conditions after it, we
  don't need containers (i.e. most rolling joins)

- Otherwise the typical case where we need containers is if we have
  something like `date >= lower, date <= upper`. Even so, the
  computation cost generally scales with the number of columns in
  `haystack` you compute containers with (here 2), and it only really
  slows down around 4 columns or so, which I haven't ever seen a real
  life example of.
