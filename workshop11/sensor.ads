package Sensor is

   type SensorMode is (Normal, Faulty);

   type Sensor is record
      Mode : SensorMode;
   end record;

   type ReadingArray is array(Positive range <>) of Float;

   procedure Initialise(ASensor : out Sensor);

   procedure GetReading(ASensor : in out Sensor; Reading : out Float);

   procedure Sort(List : in out ReadingArray);
end Sensor;
