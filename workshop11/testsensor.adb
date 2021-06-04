with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Text_Io.Editing;
use Ada.Text_Io.Editing;

with Ada.Real_Time;
use Ada.Real_Time;

with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;

with RandomFloat;
use RandomFloat;

with PreciseStats;
--with Stats;

with Sensor;

with Ada.Numerics.Float_Random;
use Ada.Numerics.Float_Random;


procedure TestSensor is
   Max : constant Integer := 10; -- the number of samples to take
   package Stats is new PreciseStats(Max);
   SensorReading : Float;
   Data : Stats.Sample;
   Low, High : Float;

   ASensor : Sensor.Sensor;

   Format : String := "<999.9999999999>";
   Pic : Picture := To_Picture(Format);
   type FixedPoint is delta 0.000000001 digits 12;
   package FixedPointOutput is new Decimal_Output(FixedPoint);
   use FixedPointOutput;

begin
   
   Sensor.Initialise(ASensor);
   
   -- get Max number of readings and add to the data
   for I in 1..Max loop
      Sensor.GetReading(ASensor, SensorReading);
      Stats.AddData(Data, SensorReading);
   end loop;

   -- print the mean, stddev, and 95% confidence interval
   Put("Mean = ");
   Put(Item => FixedPoint(Stats.Mean(Data)), Pic => Pic); New_Line;

   Put("Standard deviation = ");
   Put(Item => FixedPoint(Stats.Deviation(Data)), Pic => Pic); New_Line;
   Stats.CI(Data, Low, High);

   Put("CI = [");
   Put(Item => FixedPoint(Low), Pic => Pic);
   Put(Item => FixedPoint(High), Pic => Pic);
   Put("]"); New_Line;
end TestSensor;
