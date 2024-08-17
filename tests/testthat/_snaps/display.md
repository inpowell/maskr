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

# masked vectors have pretty printing in tibbles

    Code
      print(test_table, width = 100L)
    Output
      # A tibble: 6 x 9
        Logical    ShortInt   Integer    Double BigDouble Factor    Ordered  
        <lgl+msk> <int+msk> <int+msk> <dbl+msk> <dbl+msk> <fct+msk> <ord+msk>
      1 FALSE             8      9872      80.1      n.p. A         X        
      2 TRUE              9     11106      n.p.  9.31e-10 B         Y        
      3 FALSE            10      n.p.     100.   1   e+ 0 A         Z        
      4 TRUE           n.p.     13574     110.   1.07e+ 9 B         U        
      5 n.p.             12     14808     120.   1.15e+18 A         n.p.     
      6 NA               NA        NA      NA   NA        n.p.      <NA>     
        Character LongCharacter                                      
        <chr+msk> <chr+msk>                                          
      1 ABCDE     A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
      2 BCDEF     A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
      3 CDEFGHI   n.p.                                               
      4 n.p.      A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
      5 EFGHIJKL  A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
      6 <NA>      <NA>                                               

---

    Code
      print(test_table, width = 90L)
    Output
      # A tibble: 6 x 9
        Logical    ShortInt   Integer    Double BigDouble Factor    Ordered  
        <lgl+msk> <int+msk> <int+msk> <dbl+msk> <dbl+msk> <fct+msk> <ord+msk>
      1 FALSE             8      9872      80.1      n.p. A         X        
      2 TRUE              9     11106      n.p.  9.31e-10 B         Y        
      3 FALSE            10      n.p.     100.   1   e+ 0 A         Z        
      4 TRUE           n.p.     13574     110.   1.07e+ 9 B         U        
      5 n.p.             12     14808     120.   1.15e+18 A         n.p.     
      6 NA               NA        NA      NA   NA        n.p.      <NA>     
        Character LongCharacter                                      
        <chr+msk> <chr+msk>                                          
      1 ABCDE     A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
      2 BCDEF     A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
      3 CDEFGHI   n.p.                                               
      4 n.p.      A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
      5 EFGHIJKL  A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
      6 <NA>      <NA>                                               

---

    Code
      print(test_table, width = 80L)
    Output
      # A tibble: 6 x 9
        Logical    ShortInt   Integer    Double BigDouble Factor    Ordered  Character
        <lgl+msk> <int+msk> <int+msk> <dbl+msk> <dbl+msk> <fct+msk> <ord+ms> <chr+msk>
      1 FALSE             8      9872      80.1      n.p. A         X        ABCDE    
      2 TRUE              9     11106      n.p.  9.31e-10 B         Y        BCDEF    
      3 FALSE            10      n.p.     100.   1   e+ 0 A         Z        CDEFGHI  
      4 TRUE           n.p.     13574     110.   1.07e+ 9 B         U        n.p.     
      5 n.p.             12     14808     120.   1.15e+18 A         n.p.     EFGHIJKL 
      6 NA               NA        NA      NA   NA        n.p.      <NA>     <NA>     
      # i 1 more variable: LongCharacter <chr+msk>

---

    Code
      print(test_table, width = 60L)
    Output
      # A tibble: 6 x 9
        Logical   ShortInt Integer Double BigDouble Factor Ordered
        <lgl+msk> <int+ms> <int+m> <dbl+> <dbl+msk> <fct+> <ord+m>
      1 FALSE            8    9872   80.1      n.p. A      X      
      2 TRUE             9   11106   n.p.  9.31e-10 B      Y      
      3 FALSE           10    n.p.  100.   1   e+ 0 A      Z      
      4 TRUE          n.p.   13574  110.   1.07e+ 9 B      U      
      5 n.p.            12   14808  120.   1.15e+18 A      n.p.   
      6 NA              NA      NA   NA   NA        n.p.   <NA>   
      # i 2 more variables: Character <chr+msk>,
      #   LongCharacter <chr+msk>

---

    Code
      print(test_table, width = 40L)
    Output
      # A tibble: 6 x 9
        Logical    ShortInt   Integer   Double
        <lgl+msk> <int+msk> <int+msk> <dbl+ms>
      1 FALSE             8      9872     80.1
      2 TRUE              9     11106     n.p.
      3 FALSE            10      n.p.    100. 
      4 TRUE           n.p.     13574    110. 
      5 n.p.             12     14808    120. 
      6 NA               NA        NA     NA  
      # i 5 more variables:
      #   BigDouble <dbl+msk>,
      #   Factor <fct+msk>,
      #   Ordered <ord+msk>,
      #   Character <chr+msk>,
      #   LongCharacter <chr+msk>

---

    Code
      print(test_table, width = 100L)
    Output
      # A tibble: 6 x 9
        Logical    ShortInt   Integer    Double BigDouble Factor    Ordered  
        <lgl+msk> <int+msk> <int+msk> <dbl+msk> <dbl+msk> <fct+msk> <ord+msk>
      1 FALSE             8      9872      80.1         * A         X        
      2 TRUE              9     11106         *  9.31e-10 B         Y        
      3 FALSE            10         *     100.   1   e+ 0 A         Z        
      4 TRUE              *     13574     110.   1.07e+ 9 B         U        
      5 *                12     14808     120.   1.15e+18 A         *        
      6 NA               NA        NA      NA   NA        *         <NA>     
        Character LongCharacter                                      
        <chr+msk> <chr+msk>                                          
      1 ABCDE     A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
      2 BCDEF     A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
      3 CDEFGHI   *                                                  
      4 *         A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
      5 EFGHIJKL  A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
      6 <NA>      <NA>                                               

