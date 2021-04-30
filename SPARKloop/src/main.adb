with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Main with SPARK_Mode is
   Count: Natural := 0;
   x: Integer := 0;
begin
   while (Count < 100) loop
      Count := Count + 1;
      X := X + 5;

      -- loop invariant always be true
      -- it holds before, during and end of the loop

      -- A good loop invariant is a piece of information that
      -- 1) is true when we reach the loop;
      -- 2) holds after each loop iteration, assuming it held before
      --    that iteration;
      -- 3) holds after the loop finishes.
      pragma Loop_Invariant (X = 5 * Count);
   end loop;

   Ada.Integer_Text_IO.Put(Item => x);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put("Hello loop");
end Main;
