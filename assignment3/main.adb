pragma SPARK_Mode (On);

with StringToInteger;
with VariableStore;
with MyCommandLine;
with MyString;
with MyStringTokeniser;
with SimpleStack;
with PIN;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Long_Long_Integer_Text_IO;

procedure Main is
   DB : VariableStore.Database;
   V1 : VariableStore.Variable := VariableStore.From_String("Var1");
   PIN1  : PIN.PIN := PIN.From_String("1234");
   PIN2  : PIN.PIN := PIN.From_String("1234");
   package Lines is new MyString(Max_MyString_Length => 2048);
   S  : Lines.MyString;
   package SS is new SimpleStack(Max_Size => 512 ,Item => Integer, Default_Item => 0);
begin

   Put(MyCommandLine.Command_Name); Put_Line(" is running!");
   Put("I was invoked with "); Put(MyCommandLine.Argument_Count,0); Put_Line(" arguments.");
   for Arg in 1..MyCommandLine.Argument_Count loop
      Put("Argument "); Put(Arg,0); Put(": """);
      Put(MyCommandLine.Argument(Arg)); Put_Line("""");
   end loop;

   VariableStore.Init(DB);
   Put_Line("Adding an entry to the database");
   VariableStore.Put(DB,V1,10);

   Put_Line("Reading the entry:");
   Put(VariableStore.Get(DB,V1));
   New_Line;
   
   Put_Line("Printing out the database: ");
   VariableStore.Print(DB);
   
   Put_Line("Removing the entry");
   VariableStore.Remove(DB,V1);
   If VariableStore.Has_Variable(DB,V1) then
      Put_Line("Entry still present! It is: ");
      Put(VariableStore.Get(DB,V1));
      New_Line;
   else
      Put_Line("Entry successfully removed");
   end if;

   Put_Line("Reading a line of input. Enter some text (at most 3 tokens): ");
   Lines.Get_Line(S);

   Put_Line("Splitting the text into at most 5 tokens");
   declare
      T : MyStringTokeniser.TokenArray(1..5) := (others => (Start => 1, Length => 0));
      NumTokens : Natural;
   begin
      MyStringTokeniser.Tokenise(Lines.To_String(S),T,NumTokens);
      Put("You entered "); Put(NumTokens); Put_Line(" tokens.");
      for I in 1..NumTokens loop
         declare
            TokStr : String := Lines.To_String(Lines.Substring(S,T(I).Start,T(I).Start+T(I).Length-1));
         begin
            Put("Token "); Put(I); Put(" is: """);
            Put(TokStr); Put_Line("""");
         end;
      end loop;
      if NumTokens > 3 then
         Put_Line("You entered too many tokens --- I said at most 3");
      end if;
   end;

   If PIN."="(PIN1,PIN2) then
      Put_Line("The two PINs are equal, as expected.");
   end if;
   
   declare
      Smallest_Integer : Integer := StringToInteger.From_String("-2147483648");
      R : Long_Long_Integer := 
        Long_Long_Integer(Smallest_Integer) * Long_Long_Integer(Smallest_Integer);
   begin
      Put_Line("This is -(2 ** 32) (where ** is exponentiation) :");
      Put(Smallest_Integer); New_Line;
      
      if R < Long_Long_Integer(Integer'First) or
         R > Long_Long_Integer(Integer'Last) then
         Put_Line("Overflow would occur when trying to compute the square of this number");
      end if;
         
   end;
   Put_Line("2 ** 32 is too big to fit into an Integer...");
   Put_Line("Hence when trying to parse it from a string, it is treated as 0:");
   Put(StringToInteger.From_String("2147483648")); New_Line;
   Put(StringToInteger.From_String("ass")); New_Line;
   
   declare
      S : SS.SimpleStack;
      I : Integer;
      J : Integer;
   begin
      SS.Init(S);   
      SS.Push(S,5);   
      SS.Push(S,6);   
      SS.Pop(S,I); -- I should hold 6   
      SS.Pop(S,J); -- J should hold 5
      -- return;
      Put(I);New_Line;Put(J);
      -- SS.Pop(S,I);
      Put_Line("end");
   end;
   
   Put_Line("true end");
      
end Main;
