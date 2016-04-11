/* example4: */
 
dcl z,x int;

g: proc (t int);
   dcl y int;
   y = t * t;
   z = z + x + y;
   print(z);
end;
