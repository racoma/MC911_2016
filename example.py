/* Check Armstrong Number: */

// Armstrong number is a number which is equal to sum of digits
// raise to the power total number of digits in the number. Ex:
// 0, 1, 2, 3, 153, 370, 407, 1634, 8208

power: proc (n int, r int) returns (int);
  dcl c int, p int = 1;
  do 
    for c = 1 to r;
      p = p*n;
  od;
  return p;   
end;

dcl n int, sum int = 0;
dcl temp, remainder int, digits int = 0;
 
print("Input an integer: ");
read(n);
temp = n;
do
  while temp != 0;
    digits += 1;
    temp = temp / 10;
od;
temp = n;
do 
  while temp != 0;
    remainder = temp % 10;
    sum = sum + power(remainder, digits);
    temp = temp / 10;
od;
if n == sum then
  print(n, " is an Armstrong number.\n");
else
  print(n, " is not an Armstrong number.\n");
fi;