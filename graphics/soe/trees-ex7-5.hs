{-  
    Chapter 7 - Trees - Exercise 7.5

    Reference:
        'The Haskell School of Expression' by Paul Hudak
        http://www.cs.yale.edu/homes/hudak/SOE/index.htm
-}

{-
    Exercise 7.5
    
    Enhance the Expr data type with variables and let expressions, 
    similar in intent to Haskell’s variables and let expressions, 
    although you may assume that the let expression does not allow
    recursive definitions. Also enhance the evaluate function to 
    yield the proper value. For example:
    
        evaluate( Let "x" (C 5) (V "x" :+ V "x") ) => 10
        
    where Let and V are the new constructors in the Expr data type.
    Unbound variables should be treated as errors.
    
    (solution from: https://github.com/bwanab/SOE/blob/master/Chap7.hs)
        
-}

-- building arithmetic expressions as trees
data Expr = C     Float             -- constant
          | V     String 
          | Let   String Expr Expr
          | Expr :+ Expr
          | Expr :- Expr
          | Expr :* Expr
          | Expr :/ Expr
    deriving Show
       
e1 = (C 10 :+ (C 8 :/ C 2)) :* (C 7 :- C 4) 
e2 = Let "x" (C 5) (V "x" :+ V "x")

evaluate :: Expr -> Float
evaluate e =
    let eval :: Expr -> [(String, Float)] -> Float
        eval (C x)      vars = x
        eval (e1 :+ e2) vars = eval e1 vars + eval e2 vars
        eval (e1 :- e2) vars = eval e1 vars - eval e2 vars
        eval (e1 :* e2) vars = eval e1 vars * eval e2 vars
        eval (e1 :/ e2) vars = eval e1 vars / eval e2 vars
        eval (V s)      vars = lookup s vars
            where lookup i [] = error ("no definition for " ++ s)
                  lookup i ((a,b):vars) = if i == a 
                                          then b else lookup i vars
        eval (Let s e1 e2) vars = let es = eval e1 vars
                                  in eval e2 ((s, es):vars)
    in eval e []

ex75a = evaluate e1     -- 42.0
ex75b = evaluate e2     -- 10.0
