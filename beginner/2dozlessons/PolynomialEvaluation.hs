-- Ref: Two Dozen Short Lessons in Haskell by Rex Page
-- Chapter 15 - Modules

module PolynomialEvaluation (horner) where

horner b ds = foldr (multAdd b) 0 ds

multAdd b d s = d + b * s


