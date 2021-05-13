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

procedure main2 is
   VAR_DB : VariableStore.Database;
   VAR : VariableStore.Variable;
   MASTER_PIN  : PIN.PIN := PIN.From_String("1234");
   INPUT_PIN  : PIN.PIN;
   package Lines is new MyString(Max_MyString_Length => 2048);
   S  : Lines.MyString;
   package SS is new SimpleStack(Max_Size => 512 ,Item => Integer, Default_Item => 0);
   STACK : SS.SimpleStack;
   OPRAND : Integer;
   STATE : Integer := 0;  -- 0 == locked, 1 == unlocked
begin
   
   Put(MyCommandLine.Command_Name); Put_Line(" is running!");
   Put("I was invoked with "); Put(MyCommandLine.Argument_Count,0); Put_Line(" arguments.");
   if MyCommandLine.Argument_Count = 0 then
      Put_Line("No initial master PIN was provided, use the default PIN 1234");
   elsif MyCommandLine.Argument_Count >= 1 then
      Put_Line(MyCommandLine.Argument(1));
   end if;
   
   for Arg in 1..MyCommandLine.Argument_Count loop
      Put("Argument "); Put(Arg,0); Put(": """);
      Put(MyCommandLine.Argument(Arg)); Put_Line("""");
   end loop;
   
   VariableStore.Init(VAR_DB);
   Put_Line("Adding an entry to the database");
   
end main2;
