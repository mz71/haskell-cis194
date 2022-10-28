import Employee
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp n f) (GL l a) = GL (e : l) (a+f)