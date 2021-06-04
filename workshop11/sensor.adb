with RandomFloat;
use RandomFloat;

with Ada.Text_IO;

package body Sensor is

   MU : constant Float := 60.0;
   SIGMA : constant Float := 1.0;

   TO_FAILURE : constant Float := 0.02;

   procedure Initialise(ASensor : out Sensor) is
   begin
      ASensor.Mode := Normal;
   end Initialise;

   procedure GetReading(ASensor : in out Sensor; Reading : out Float) is
   begin
      if ASensor.Mode = Normal then
         Reading := NormalDistribution(MU, SIGMA);
      else
         Reading := UniformDistribution(MU - 20.0, MU + 20.0);
      end if;
      if UniformDistribution(0.0, 1.0) <= TO_FAILURE then
         ASensor.Mode := Faulty;
      end if;
   end GetReading;

   procedure Sort(List : in out ReadingArray) is
   begin

      for I in List'First .. List'Last-1 loop

         -- List (List'First..I-1) are the smallest elements
         -- of List in order

         declare
            Temp : Float := List (I);
            Min_Index : Integer := I;
         begin

            -- Find index of next smallest element
            for J in I+1 .. List'Last loop
               if List(J)<List(Min_Index) then
                  Min_Index := J;
               end if;
            end loop;
            -- Exchange List (Min_Index) and List (I)
            List (I) := List (Min_Index);
            List (Min_Index) := Temp;
         end;

      end loop;

   end Sort;
end Sensor;
