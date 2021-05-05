package body DivMod with SPARK_Mode is

   procedure DivMod(X : Positive; N : Positive; K : out Natural; Remainder : out Natural)
   is
      Y : Natural := X;
   begin
      K := 0;
      while Y >= N loop
         Y := Y - N;
         K := K + 1;
      end loop;
      Remainder := Y;
   end DivMod;


end DivMod;
