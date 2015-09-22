my_append [] q r = (q=:=r)
my_append (x:xs) q (x:ys) = my_append xs q ys