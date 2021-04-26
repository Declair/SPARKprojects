with Ada.Text_IO;
with Ada.Integer_Text_IO;
with SimpleStack;

procedure Main with SPARK_Mode is
   -- local variable go here
   package SS is new SimpleStack(100, Integer, 0);
   S : SS.SimpleStack;
   I: Integer;
   J: Integer;

begin
   Ada.Text_IO.Put("Hello world");
   Ada.Text_IO.New_Line;

   SS.Init(S);

   SS.Push(S, 5);
   SS.Push(S, 7);

   SS.Pop(S, I);
   SS.Pop(S, J);

   Ada.Integer_Text_IO.Put(J);
   Ada.Integer_Text_IO.Put(I);
end Main;
