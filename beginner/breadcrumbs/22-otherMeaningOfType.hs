type Name = String
-- this aliases Name to be a special kind of String


-- for instance:
whatsMyName :: Name  -- remember type specifications?
whatsMyName = "Sasquatch"

-- Note: 'type' is used to provide synonyms or aliases for other
--       types; often used to give constructor or function arguments
--       application specific names ie Radix as Float, Point as (Int,Int)