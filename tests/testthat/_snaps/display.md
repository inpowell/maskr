# suppression hides selected values

    Code
      suppressed(c(1L, 2L, NA, NA), c(FALSE, TRUE, FALSE, TRUE))
    Output
      <maskr_suppressed[4]>
         1 n.p.   NA n.p.

---

    Code
      suppressed(c(1, 2, NA, NA), c(TRUE, FALSE, TRUE, FALSE))
    Output
      <maskr_suppressed[4]>
      n.p.    2 n.p.   NA

---

    Code
      suppressed(c("1", "2", NA, NA), c(TRUE, FALSE, FALSE, TRUE))
    Output
      <maskr_suppressed[4]>
      n.p. 2    NA   n.p.

