# can't get common type of Date and IDate

    Code
      vec_ptype2(x, y)
    Condition
      Error:
      ! Can't combine `x` <date> and `y` <IDate>.

---

    Code
      vec_ptype2(y, x)
    Condition
      Error:
      ! Can't combine `y` <IDate> and `x` <date>.

# can't cast Date to IDate

    Code
      vec_cast(x, y)
    Condition
      Error:
      ! Can't convert `x` <date> to <IDate>.

# can't cast IDate to Date

    Code
      vec_cast(x, y)
    Condition
      Error:
      ! Can't convert `x` <IDate> to <date>.

