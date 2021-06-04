with Ada.Numerics.Elementary_Functions;  use Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;                        use Ada.Text_IO;

package body PreciseStats is

   -- 95% confidence interval (95% of the area of a normal
   -- distribution curve lie with 1.96 standard deviations of the
   -- mean)
   CONFINT : constant Float := 1.96;

   -- 99% confidence interval (falls within 2.58 stddevs of mean)
   --CONFINT : constant Float := 2.58;

   procedure AddData (Data : in out Sample; Point : Float) is
   begin
      Data.Data(Data.N + 1) := Point;
      Data.N := Data.N + 1;
   end AddData;

   function Mean (Data: in Sample) return Float is
      Sum : Float := 0.0;
   begin
      for I in 1..Data.N loop
         Sum := Sum + Data.Data(I);
      end loop;
      return Sum / Float(Data.N);
   end Mean;

   function Deviation (Data : in Sample) return Float is
      SMean : Float;
      Sum : Float := 0.0;
   begin
      SMean := Mean(Data);
      for I in 1..Data.N loop
         Sum := Sum + ((Data.Data(I) - SMean) ** 2);
      end loop;
      return Sqrt(Sum / Float(Data.N - 1));
   end Deviation;

   -- Return the 95% confidence interval
   procedure CI (Data : in Sample; Low : out Float; High : out Float) is
      SMean : Float;
      SDeviation : Float;
   begin
      SMean := Mean(Data);
      SDeviation := Deviation(Data);
      Low := SMean - CONFINT * (SDeviation / Sqrt(Float(Data.N)));
      High := SMean + CONFINT * (SDeviation / Sqrt(Float(Data.N)));
   end CI;

end PreciseStats;
