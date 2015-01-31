-- Chapter 7 - Predicate Logic - Exercises
--
-- Reference: 'Discrete Mathematics Using a Computer' by John O'Donnell,
--             Cordelia Hall, and Rex Page
import Stdm

{-
    Exercise 1
    Let the Universe = {1,2,3}. Expand the following expressions
    i.e. remove the quantifiers
    
    (a) (all x).F(x)
            F(1) && F(2) && F(3)
            
    (b) (exists x).F(x)
            F(1) || F(2) || F(3)
            
    (c) (exists x).(all y).G(x,y)
            (all y).G(1,y)
         || (all y).G(2,y)
         || (all y).G(3,y)
         
            (G(1,1) && G(1,2) && G(1,3))
         || (G(2,1) && G(2,2) && G(2,3))
         || (G(3,1) && G(3,2) && G(3,3))

-}
{-
    Exercise 2
        Let the universe be the set of integers. Expand the 
        following expression:
        
        (all x) in {1,2,3,4}.(exists y) in {5,6}.F(x,y)
        
            (F(1,5) || F(1,6))
         && (F(2,5) || F(2,6))
         && (F(3,5) || F(3,6))
         && (F(4,5) || F(4,6))
                  
-}
{-
    Exercise 3
        Express the following statements formally using the universe
        of 'natrual numbers' and where E(x) means 'x is even' and
        O(x) means 'x is odd'
        
        There is an even number.        
                (exists x).E(x)
                
        Every number is either even or odd.
                (all x).(E(x) || O(x))      -- need the parentheses
                                            -- for proper scope
                
        No number is both even and odd.
                not(exists x).E(x) && O(x)
                
                Solution:  (all x).not(E(x) && O(x))
                
        The sum of two odd numbers is even.
            (all x,y)in O.E(x+y)
            
            Solution:  (all x).(all y).(O(x) && O(y)) -> E(x+y))
            
        The sum of an odd number and an even number is odd.
            (all x) in O.(exists y) in E.O(x+y)
            
            Solution: (all x).(all y).(E(x) && O(y) -> O(x+y))
        
-}
{-
    Exercise 4
        Let the universe be all animals and define the following:
        
        B(x)    = x is a bird
        D(x)    = x is a dove
        C(x)    = x is a chicken
        P(x)    = x is a pig
        F(x)    = x can fly
        W(x)    = x has wings
        M(x,y)  = x has more feathers than y does
        
        Translate the following sentences into logic.
        
        Chickens are birds.
            (all x).C(x) -> B(x)
        
        Some doves can fly.
            (exists x).D(x) && F(x)
        
        Pigs are not birds.
            (all x).P(x) -> not B(x)
        
        Some birds can fly, and some can't.
            (all x).(B(x) -> (F(x) || not F(x)))
            
            Solution:    ((exists x).B(x) && F(x))
                      && ((exists x).B(x) && not F(x))
        
        An animal needs wings in order to fly.
            (all x).W(x) -> F(x)
            
            Solution: (all x).not W(x) -> not F(x)
        
        If a chicken can fly, then pigs have wings.
            (exists x).(all y).(C(x) && F(x) -> (P(y) && W(y))
            
            Solution:   ((exists x).C(x) && F(x))
                     -> ((all x).P(x) -> W(x))
        
        Chickens have more feathers than pigs do.
            (all x)C.(all y)P.M(x,y)
            
            Solution: (all x,y).(C(x) && P(y)) -> M(x,y)
        
        An animal with more feathers than any chicken can fly.
            (all x,y).(C(x) && not C(y) && not M(x,y) -> F(y))
            
         Solution: (all x).((A(x) && ((all y).(C(y) && M(x,y)))) -> F(x))
-}
{-
    Exercise 5
        Translate the following into English
        
        (all x).((exists y).wantsToDance with(x,y))
        
            For every person, there is at least one other person they 
            want to dance with.
            
            Solution: Everybody (all x) has someone (exists y) they 
                      want to dance with.
        
        (exists x).((all y).wantsToPhone(y,x))
        
            For every person, there is at least one other person who 
            wants to phone them.
            
            Solution: There is someone (x) whom everyone (all y)
                      wants to call.
        
        (exists x).(tired x && (all y).helpsMoveHouse(x,y))
        
            There exists one person who is tired and still wants
            to help any other person move house.
            
            Solutions: There is a person (exists x) who is tired,
                       and who helps everyone (all y) to move house.
                       
        (exists -> someone, a person, etc.)
        (all    -> everyone, everybody, etc.)
        
-}
{-
    Exercise 6
        Write the predicate logic expressions corresponding to the
        following Haskell expressions. Determine their values and
        check using Hugs.
        
        (Note: 'forall' is in Stdm.lhs)
-}    
ex6a = forall [1,2,3] (==2)     -- (all x).(x == 2)   False
ex6b = forall [1,2,3] (< 4)     -- (all x).(x < 4)    True
ex6 = ex6a == False && ex6b == True

{-
    Exercise 7
        Write the predicate logic expressions corresponding
        to the following Haskell expressions; determine their
        values and check using Hugs.
-}
ex7a = exists [0,1,2] (== 2)    -- (exists x).(x == 2)   True
ex7b = exists [1,2,3] (> 5)     -- (exists x).(x > 5)    False
ex7 = ex7a == True && ex7b == False

{-
    Exercise 8
        Define the predicate p(x,y) to mean x = y + 1 and let
        the universe be {1,2}. Calculate the value of each of
        the following expressions and check using Hugs.
        
        (a) (all x).(exists y).p(x,y)       False
        (b) (exists x,y).p(x,y)             True
        (c) (exists x).((all y).p(x,y))     False
        (d) (all x,y).p(x,y)                False
-}
yVals = map (+1) [1,2] :: [Int]     -- y values

pe x = exists yVals (== x)          -- predicate for 'exists'
pa x = forall yVals (== x)          -- predicate for 'forall'

ex8a = forall [1,2] pe              -- False
ex8b = exists [1,2] pe              -- True
ex8c = exists [1,2] pa              -- False
ex8d = forall [1,2] pa              -- False

