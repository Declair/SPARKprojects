with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Ada.Text_IO;
use Ada.Text_IO;
with DivMod;


procedure Main with SPARK_Mode is
   K : Natural;  -- quotient
   R : Natural;  -- remainder
   X : Integer;  -- dividend
   N : Integer;  -- divisor
begin
   Put_Line("Compute the result and remainder of X/N, via repeated subtraction.");
   Put_Line("The result and remainder are numbers K and R where X = K * N + R and R < N");
   Put("Enter a positive integer for X: ");
   Get(X);
   Put("Enter a positive integer for N: ");
   Get(N);
   if (X > 0 and N > 0) then
      DivMod.DivMod(X,N,K,R);
      Put("K: "); Put(K); New_Line;
      Put("R: "); Put(R); New_Line;

      -- assert that the result is correct
      pragma Assert (X = K * N + R and R < N);

      -- SPARK needs some help to conclude that X/N = K.
      -- We derive that final result step-by-step

      -- firstly K is not greater than X/N
      pragma Assert (K * N <= X);
      pragma Assert (X/N >= K);

      -- secondly K is not lower than X/N
      pragma Assert (if K < Integer'Last and then Integer'Last / N > K + 1 then
      --                   N * (K + 1) > X);
      pragma Assert (X/N <= K);

      -- therefore K is exactly equal to X/N
      pragma Assert (X/N = K);
   else
      Put_Line("X and N must both be positive. Exiting.");
   end if;
end Main;


