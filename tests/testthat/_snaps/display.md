# mask hides selected values

    Code
      masked(c(1L, 2L, NA, NA), c(FALSE, TRUE, FALSE, TRUE))
    Output
      <integer+masked[4]>
         1 n.p.   NA n.p.

---

    Code
      masked(c(1, 2, NA, NA), c(TRUE, FALSE, TRUE, FALSE))
    Output
      <double+masked[4]>
      n.p.    2 n.p.   NA

---

    Code
      masked(c("1", "2", NA, NA), c(TRUE, FALSE, FALSE, TRUE))
    Output
      <character+masked[4]>
      n.p. 2    NA   n.p.

