-- Chapter 9 - Inductively Defined Sets
--
-- Reference: 'Discrete Mathematics Using a Computer' by John O'Donnell,
--             Cordelia Hall, and Rex Page

{-
    There are different ways to define a set:
    
    enumeration     - simplest, just name the elements
                    - ok for small, finite sets
                    - impractical for large sets
    ellipses        - use of ellipses to indicate the set continues
                    - imprecise
                    
    When we use 'induction' to show a value 'v' is in a set, we first
    enumerate the values that come before it; these values form a
    'sequence' which is simply a set with an ordering.
    
    
-}