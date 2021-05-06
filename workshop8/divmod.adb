with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Ada.Text_IO;
use Ada.Text_IO;

package body DivMod with SPARK_Mode is

   procedure DivMod(X : Positive; N : Positive; K : out Natural; Remainder : out Natural)
   is
      Y : Natural := X;
   begin
      K := 0;
      while Y >= N loop
         Y := Y - N;
         K := K + 1;
         -- Put("Y: "); Put(Y);New_Line;
         -- Put("K: "); Put(K);New_Line;

         pragma Loop_Invariant (X = Y + K * N);
      end loop;
      Remainder := Y;
   end DivMod;


end DivMod;
