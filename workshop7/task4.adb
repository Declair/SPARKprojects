package body Task4 with
SPARK_Mode => On
is

   procedure Task4Procedure(AnArray : in out MyArray; AnIndex : in Index) is
   begin
      AnArray(AnIndex) := AnArray(AnIndex) + 1;
   end Task4Procedure;
end Task4;

