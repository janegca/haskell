{-
    Chapter 3 - The Use of Logic: Proofs
    
    References:
        "The Haskell Road to Logic, Maths and Programming, 2nd Edition"
        by Kees Doets, Jan van Eijck, College Publications, UK, 2012
        
-}
-- 3.3 Rules for Connectives --------------------------------------------
{-   
    Implication
    -----------
                    P  Q  P => Q
                    ------------
                    T  T    T   
                    T  F    F   
                    F  T    T   
                    F  F    T   
        
        Introduction Rule (Deduction Rule) 
            allows you to reduce the problem of  P => Q by
            assuming P is True as a new given from which you
            now derive Q.  
            
            If P is False, P => Q will be true anyway
            (from implies truth table). That leaves us to 
            consider what happens if P is True, which means
            we can assume it is a 'given' from which we will
            infer Q, which we still have to prove is True.
            
        Elimination Rule  (Modus Ponens)
            now that we have P as a given and P => Q, if follows
            that Q must be True.
            
            i.e. if we assume P and P => Q are both True then the only
                 possible value for Q is also True
                 
                    P => Q  &&  P
                    -------------
                    t  T t   T  t  <--- Q MUST be true if the Givens are
                    t  F f   F  t         True
                    
        Example Proof:
        
            Given:        P => Q, Q => R    [assumed both are True]
            To be proved: P => R
            Proof:
                Suppose P [assume P is True]
                From P => Q and P, conclude Q  [must be True 'modus ponens']
                From Q => R and Q, conclude R  [modus ponens]
                Thus, P => R  [ P => Q => R so P => R]
                
        Exercise 3.2
            Apply both implication rules to prove P => R from the givens
            P => Q, P => (Q => R)
            
            Given:        P => Q, P => (Q => R)       [assumed True]
            To be proven: P => R
            Proof:
                Suppose P                           [assume P is True]
                From P => Q and P, conclude Q       [modus ponens]
                                                [missed a step, see below]
                Then from Q => R and Q, conclude R  [modus ponens]
                Therefore P => R  
        
        Provided Solution:
            Given:        P => Q, P => (Q => R).
            To be proved: P => R.
            Proof:
                Suppose P.
                To be proved: R.
                Proof:
                    From P => Q and P we get Q.
                    From P => (Q => R) and P we get Q => R.
                    From Q => R and Q we get R.
                    Thus P => R
-)
{-
    Conjunction
    -----------
                    P  Q  P && Q
                    ------------
                    T  T     T  
                    T  F     F  
                    F  T     F  
                    F  F     F  
                    
    Introduction Rule
        follows from the truth table, if P and Q are both 'given'
        i.e. assumed to be True then P && Q is True
        
    Elimination Rule
        if P && Q is True then both P and Q, individually, are True
        
    Example: 
        Assume that m and n are Natural numbers.
        To show: (m is even && n is even) => m + n is even
        ie. Prove that the sum of even numbers is also even.
    
        Given:        m and n are natural numbers
        To be proved: that if m and n are even, the sum of m and n is even
        Proof:
            Assume m and n are even numbers
            For instance, m = 2p, n = 2q, p and q are natural numbers
            Then m + n = 2p + 2q = 2(p + q) is even
            
    Exercise 3.4
        Assume that n and m are natural numbers.
        Show: (m is odd /\ n is odd) => m + n is even
        Proof:
            Assume m and n are odd numbers
            For instance, m = 2p+1, n = 2q + 1, p and q are natural numbers
            Then m + n = 2p + 2q + 2 = 2(p + q + 1) is even
                    
-}                    