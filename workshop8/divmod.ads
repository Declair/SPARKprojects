package DivMod with SPARK_Mode is

   procedure DivMod(X : in Positive; N : in Positive; K : out Natural;
                    Remainder : out Natural) with
   Post => (K * N + Remainder = X and Remainder < N);

end DivMod;
