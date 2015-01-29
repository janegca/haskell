{-
    Chapter 3 - The Use of Logic: Proofs
    
    Basic Guidelines for Proofs
        1. Concentrate on the form of what is to be proved
        2. Simplify the proof problem as much as possible using
           the rules
        3. After simplifying the problem as much as possible,
           look to the givens to see which you can use
        4. It is usually a good idea to move negation inward as
           much as possible before applying the not-introduction rule
        5. Stay away from Proof by Contradiction as long as possible
    
    References:
        "The Haskell Road to Logic, Maths and Programming, 2nd Edition"
        by Kees Doets, Jan van Eijck, College Publications, UK, 2012
        
        Also see 'Discrete Math' Chapter 6
        
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
{-
    Equivalence
    -----------
        An 'equivalence' (iff) can be thought of as the conjunction (&&)
        of two implications (=>) so the rules from those for => and &&
        
    Introduction Rule
        To prove P <=> Q you need to:
        (1) add P as a new given and prove Q    [P 'only if' Q]
        (2) add Q as a new given and prove P    [Q 'if' P]
        
    Elimination Rule (from Implication Elimination rule, modus ponens)
        Given P <=> Q, P, thus Q
        Given P <=> Q, Q, thus P
        
    Exercise 3.5
        Show:
        1. From P <=> Q it follows that (P => R) <=> (Q => R)
        
        Provided Solutions:
        Assume P <=> Q
            Suppose P => R
            Assume Q
            Then from Q, P <=> Q we get P and from P, P => R, we get R
            Thus Q => R
            Suppose Q => R
            Assume P
            Then from P, P <=> Q we get Q and from Q, Q => R, we get R
            Thus P => R
            Thus (P => R) <=> (Q => R)
        
        
        2. From P <=> Q it follows that (R => P) <=> (R => Q)
        
        Assume P <=> Q
            Suppose R => P
            Assume R
            Then from R => P we get P and from P <=> Q we get Q
            Thus R => Q
            Suppose R => Q
            Assume R
            Then from R => Q we get Q and from P <=> Q we get {
            Thus R => P
            Thus (R => P) <=> (R => Q)
            
-}
{-
    Negation
    --------
        As a general rule, try to remove 'negation' by converting
        the statement into something positive; then use the other
        proof rules. 
        
        Introduction Rule
            If (not P) is to be proved, assume P as a new given
            and then try to prove something (depending on the
            context) that is evidently false.
            
            An 'evidently false' statement is one that is patently
            untrue i.e. 1 = 0

    Exercise 3.7 
        Produce Proofs for:
        1. Given: P => Q, show: (not Q) => (not P)
        
        Provided Solution:
        Assume (not Q)
            Assume P
            Then from P => Q and P we get Q which contradicts (not Q)
            Thus (not P)
            Thus (not Q) => (not Q)
        
        2. Give P <=> Q, show, (not P) <=> (not Q)
        
        Assume (not P)
            Assume Q
            Then from P <=> Q, Q we get P which contradicts (not P)
            Thus (not P) => (not Q)
            
        Assume (not Q)
            Assume P
            Then from P <=> Q, P we get Q which contradicts (not Q)
            Thus (not Q) => (not P)
        So (not P) <=> (not Q)
        
    Elimination Rule
        If trying to prove (not P), on the basis of the other given,
        you can attempt to prove P must hold. In which case the
        elimination rule declares the proof problem to be solved,
        'no matter' the statements to be proved!
        
        Note: This has to be 'safe' as there is no conditon in which
              P is also (not P)
              
    Proof by Contradiction (Reduction ad Absurdum)
        In order to prove P, add (not P) as a given and attempt
        to prove an evidently false statement.
        
        NOTE: Beginner's tend to overuse this rule and lead themselves
              into a tangle mess; use if only when all other options
              fail.
              
              Don't confuse it with the not-Introduction Rule and
              remember it is usually better to move 'not' inside
              instead of applying the not-introduction rule.
              
    Example 3.8
        From (not Q) => (not P) it follows that P => Q
        
        Given:  (not Q) => (not P)
        Prove:  P => Q
        
        Suppose P
            Prove: Q
                Suppose (not Q)
                Prove: False
                    From (not Q) and (not Q) => (not P) derive (not P)
                    From P and (not P) derive False
                Thus Q by contradiction
            Thus P => Q by the Deduction Rule
            
    Exercise 3.9
        Show that from (P => Q) => P it follows that P using 
        Proof by Contradiction
        
        Provided solution:
        Given: (P => Q) => P
        Prove: P
        Assume (not P)
        If, (P => Q) then  from the given, P, a contradiction
        So not(P => Q)
        But then P, and contradiction with assumption (not P)
        Thus, P
        
-}
{-
    Disjunction
    -----------
        Introduction Rule
            A disjunction follows from each disjunct
            
            Given P, thus P || Q
            Given Q, thus P || Q
            
        Elimination Rule
            You can use a given P || Q by giving two proofs, one
            using P, the other using Q
            
    Example 3.10
        Show that from P || Q, not P it follows that Q
        
        Given: P || Q, not P
        Prove: Q
        
        Suppose P, then from P and (not P) we get Q
        Suppose Q, then Q holds by assumption.
        Therefore, Q
        
    Exercise 3.11
        Assume A,B,C, and D are statements
        1. From the given A => B || C and B => not A, derive that
           A => C
           
           Given: A => B || C, B => (not A)
           Prove: A => C
           
           Suppose B, then A => B and B => (not A) gives C, so A => C
           Suppose C, then A => C by assumption
           Therefore A => C
           
           Provided Solution:
            Suppose A
            Prove: C
            From A and A => B || C, we get B || C
            If B then from the given B => not A we get not A and 
            contradiction, so (not B)
            From B || C and (not B) we get C from the reasoning in Ex 3.10
            Thus A => C
            
        2. From the given A || B => C || D, C => A and B => not A, derive
           that B => D
           
           Given: A || B => C || D, C => A and B => A
           Prove: B => D
           
           Suppose B
           Prove: D
           From B and A || B => C we get B => C || D 
           Then B, and B => D give D
           
           Suppose A
           Prove: B => D
           From A and A || B => C || D we get A => C || D
           Suppose D, then A => D but this contradicts C => A as C => A
           and A gives C therefore A => (not D)
           So B => D
           
           Provided Solution:
            Assume B
            Prove: B => D
            From B, B => not A, (not A)
            From (not A) and C => A we get (not C)
            From B we get A || B and with A || B => C || D we get C || D
            From reasoning in 3.10, from C || D and (not C) we get D
            Thus B => D
-}
{-
    Universal Quantifier
    --------------------
        Introduction Rule
        
        When asked to prove 'all x have property E(x)', start by
        supposing that 'c' is an 'arbitrary object'
        
        When asked to prove 'all x in the set A have property E(x)'
        start by supposing that 'c' is an 'arbitray object' in A
        
        Where an 'arbitrary object' is 'something unspecified about
        which no special assumptions are made' i.e. an arbitrary
        object is any random chosen member of the given domain
        for example, any natural number from the set of natural numbers
        
        Elimination Rule
            Given that all 'x' have the property E(x) then
            E(t) where 't', as a member of 'all x' which have the property
-}
{-
    Existential Quantifier
    ----------------------
        Introduction Rule
            In order to show that at least one 'x' has the property
            E(x) you need only show that E(t) hold for one member, t
            
            It is not always possible to come up with an example
            object; especially when using Proof by Contradiction
            of existential statements (a good reason to stay away
            from this rule if at all possible)
            
        Elimination Rule
            When you want to prove that at least one member has
            a property E(x), suppose that 'c' is an object that
            satisfies E (and that is all you can suppose about 'c')
            
            Similarly, if you to prove at least one member of a set
            satisfies E(x) then suppose that 'c' is an object in set
            A that satisfies E (and that is all you can suppose about 'c')
            
-}
