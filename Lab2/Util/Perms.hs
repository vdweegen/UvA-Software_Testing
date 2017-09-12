module Lab2.Util.Perms where

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
              insrt x [] = [[x]]
              insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)