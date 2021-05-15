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

-- with GNAT.OS_Lib;

procedure main2 is
   VAR_DB : VariableStore.Database;
   --VAR : VariableStore.Variable;
   MASTER_PIN  : PIN.PIN := PIN.From_String("1234");
   INPUT_PIN  : PIN.PIN;
   package Lines is new MyString(Max_MyString_Length => 2048);
   S  : Lines.MyString;
   package SS is new SimpleStack(Max_Size => 512 ,Item => Integer, Default_Item => 0);
   STACK : SS.SimpleStack;
   --OPRAND : Integer;
   STATE : Integer := 0;  -- 0 == locked, 1 == unlocked
begin
   
   Put(MyCommandLine.Command_Name); Put_Line(" is running!");
   
   -- Receiving and setting master PIN from commandline argument
   if MyCommandLine.Argument_Count = 0 then
      -- TODO: make sure what to do if the program is invoked with no argument
      Put_Line("No initial master PIN was provided, use the default PIN 1234");
   elsif MyCommandLine.Argument_Count >= 1 then
      
      if MyCommandLine.Argument(1)' Length = 4 and
        (for all I in MyCommandLine.Argument(1)'Range => 
             MyCommandLine.Argument(1)(I) >= '0' and 
             MyCommandLine.Argument(1)(I) <= '9') then
         MASTER_PIN := PIN.From_String(MyCommandLine.Argument(1));
      else
         Put_Line("Invalid initial master PIN.");
         return;
      end if;
   end if;
   Put_Line("Finish setting the initialise master PIN");
   
   VariableStore.Init(VAR_DB);
   Put_Line("Finish Initialising var database");
   
   SS.Init(STACK);
   Put_Line("Finish Initialising oprand stack");
   
   while True loop
      if STATE = 1 then
         Put("unlocked> ");
      else
         Put("locked> ");
      end if;
      Lines.Get_Line(S);
   end loop;
   
end main2;
