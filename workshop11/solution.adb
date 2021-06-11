with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Text_Io.Editing;
use  Ada.Text_Io.Editing;

with Sensor;
with PreciseStats;

with Ada.Numerics.Float_Random;
use  Ada.Numerics.Float_Random;

procedure solution is   
   Max : constant Integer := 10; -- the number of samples to take   
   package Stats is new PreciseStats(Max);   
   FinalReading : Float;   
   DataFirstSensor, DataMedian : Stats.Sample;
   Low, High : Float;
   
   Format : String := "<999.9999999999>";
   Pic : Picture := To_Picture(Format);
   type FixedPoint is delta 0.000000001 digits 12;
   package FixedPointOutput is new Decimal_Output(FixedPoint);
   use FixedPointOutput;
   
   NUM_SENSORS : constant Integer := 1000;
   type SensorArray is array(1..NUM_SENSORS) of Sensor.Sensor;
   Sensors : SensorArray;
   Readings : Sensor.ReadingArray(1..NUM_SENSORS);
begin
   for J in 1..NUM_SENSORS loop
      Sensor.Initialise(Sensors(J));
   end loop;
   
   for I in 1..Max loop
      for J in 1..NUM_SENSORS loop
         Sensor.GetReading(Sensors(J), Readings(J));
      end loop;
      Stats.AddData(DataFirstSensor, Readings(1));
      Sensor.Sort(Readings);
      FinalReading := Readings(NUM_SENSORS/2 + 1);
      Stats.AddData(DataMedian, FinalReading);
   end loop;
   
   Put("Mean First  = ");
   Put(Item => FixedPoint(Stats.Mean(DataFirstSensor)), Pic => Pic); New_Line;
   
   Put("Mean Median = ");
   Put(Item => FixedPoint(Stats.Mean(DataMedian)), Pic => Pic); New_Line;
   
   Put("Standard deviation First = ");
   Put(Item => FixedPoint(Stats.Deviation(DataFirstSensor)), Pic => Pic); New_Line;
   Put("Standard deviation Median = ");
   Put(Item => FixedPoint(Stats.Deviation(DataMedian)), Pic => Pic); New_Line;
   
   Stats.CI(DataFirstSensor, Low, High);
   Put("CI First =  [");
   Put(Item => FixedPoint(Low), Pic => Pic);
   Put(Item => FixedPoint(High), Pic => Pic);
   Put("]"); New_Line;
   
   Stats.CI(DataMedian, Low, High);
   Put("CI Median = [");   Put(Item => FixedPoint(Low), Pic => Pic);
   Put(Item => FixedPoint(High), Pic => Pic);
   Put("]"); New_Line;
   
end solution;
