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

with Ada.Containers; use Ada.Containers;  -- to check if variable store is full


procedure main2 is
   LOCKED : constant Integer := 0;
   UNLOCKED : constant Integer := 1;
   VAR_DB : VariableStore.Database;
   MASTER_PIN  : PIN.PIN := PIN.From_String("1234");
   package Lines is new MyString(Max_MyString_Length => 2048);
   T : MyStringTokeniser.TokenArray(1..5) := (others => (Start => 1, Length => 0));
   package OprandStack is new SimpleStack(Max_Size => 512, Item => Integer, Default_Item => 0);
   STACK : OprandStack.SimpleStack;
   STATE : Integer := Locked;
   INVALID : Integer := 0;
   
   procedure Execute is
      S  : Lines.MyString;
      NumTokens : Natural;
   begin
      Lines.Get_Line(S);
      MyStringTokeniser.Tokenise(Lines.To_String(S),T,NumTokens);
      -- Put("You entered "); Put(NumTokens); Put_Line(" tokens.");
      
      if NumTokens = 0 then
         return;
      end if;
      
      if Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)) = "unlock" then
         if STATE = UNLOCKED then
            return;
         end if;
         
         if NumTokens /= 2 then
            INVALID := 1;
            Put_Line("Expect 2 arguments.");
            return;
         end if;
         
         declare
            Str : String := Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1));
         begin
            if not (Str'Length = 4 and
              (for all I in Str'Range => Str(I) >= '0' and Str(I) <= '9')) then
               -- INVALID := 1;
               Put_Line("Invalid PIN");
               return;
            end if;
            
            if PIN."="(MASTER_PIN, PIN.From_String(Str)) then
               STATE := UNLOCKED;
            else
               -- try to unlock with wrong PIN
               return;
            end if;
         end;
               
      end if;
      
      if Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)) = "lock" then
         if STATE = LOCKED then
            return;
         end if;
         
         if NumTokens /= 2 then
            INVALID := 1;
            Put_Line("Expect 2 arguments.");
            return;
         end if;
         
         declare
            Str : String := Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1));
         begin
            if not (Str'Length = 4 and
              (for all I in Str'Range => Str(I) >= '0' and Str(I) <= '9')) then
               -- INVALID := 1;
               Put_Line("Invalid PIN");
               return;
            end if;
            
            STATE := LOCKED;
            MASTER_PIN := PIN.From_String(Str);
         end;
      end if;
      
      if Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)) = "load" then
         if STATE = LOCKED then
            return;
         end if;
         
         if NumTokens /= 2 then
            INVALID := 1;
            Put_Line("Expect 2 arguments.");
            return;
         end if;
         
         declare
            Str : String := Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1));
            
         begin
            if Str'Length > VariableStore.Max_Variable_Length then
               Put_Line("The length of the variable is too long!");
               return;
            end if;
            
            if OprandStack.Size(STACK) >= OprandStack.Capability then
               Put_Line("The stack is full!");
               return;
            end if;
            
            declare
               VAR : VariableStore.Variable := VariableStore.From_String(Str);
            begin
               Put("Looking up "); Put_Line(Str);
            
               if not VariableStore.Has_Variable(VAR_DB, VAR) then
                  Put_Line("Entry not found!");
                  return;
               end if;
               OprandStack.Push(STACK, VariableStore.Get(VAR_DB, VAR));
            end;
            
         end;
         
      end if;
      
      if Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)) = "store" then
         if STATE = LOCKED then
            return;
         end if;
         
         if NumTokens /= 2 then
            INVALID := 1;
            Put_Line("Expect 2 arguments.");
            return;
         end if;
         
         declare
            Str : String := Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1));
         begin
            if OprandStack.Size(STACK) = 0 then
               Put_Line("The stack is empty!");
               return;
            end if;
         
            if Str'Length > VariableStore.Max_Variable_Length then
               Put_Line("The length of the variable is too long!");
               return;
            end if;
            
            declare
               VAR : VariableStore.Variable := VariableStore.From_String(Str);
               I : Integer;
            begin
               if not (VariableStore.Length(VAR_DB) < VariableStore.Max_Entries or
                      VariableStore.Has_Variable(VAR_DB, VAR)) then
                  Put_Line("The variable database is full.");
                  return;
               end if;
               OprandStack.Pop(STACK, I);
               VariableStore.Put(VAR_DB, VAR, I);
            end;
            
         end;
         
      end if;
      
      if Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)) = "remove" then
         if STATE = LOCKED then
            return;
         end if;
         
         if NumTokens /= 2 then
            INVALID := 1;
            Put_Line("Expect 2 arguments.");
            return;
         end if;
         
         declare
            Str : String := Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1));
         begin
            if Str'Length > VariableStore.Max_Variable_Length then
               Put_Line("The length of the variable is too long!");
               return;
            end if;
            
            declare
               VAR : VariableStore.Variable := VariableStore.From_String(Str);
            begin
               if VariableStore.Has_Variable(VAR_DB, VAR) then
                  VariableStore.Remove(VAR_DB, VAR);
               end if;
            end;
            
         end;
      end if;
      
      if Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)) = "list" then
         if STATE = LOCKED then
            return;
         end if;
         
         if NumTokens /= 1 then
            INVALID := 1;
            Put_Line("Expect 1 arguments.");
            return;
         end if;
         
         VariableStore.Print(VAR_DB);
      end if;
      
      if Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)) = "push" then
         if STATE = LOCKED then
            return;
         end if;
         
         if NumTokens /= 2 then
            INVALID := 1;
            Put_Line("Expect 2 arguments.");
            return;
         end if;
         
         if OprandStack.Size(STACK) >= OprandStack.Capability then
            Put_Line("The stack is full!");
            return;
         end if;
         
         declare
            Str : String := Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1));
         begin
            OprandStack.Push(STACK, StringToInteger.From_String(Str));
         end;
         
      end if;
      
      if Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)) = "pop" then
         if STATE = LOCKED then
            return;
         end if;
         
         if NumTokens /= 1 then
            INVALID := 1;
            Put_Line("Expect 1 arguments.");
            return;
         end if;
         
         if OprandStack.Size(STACK) = 0 then
            Put_Line("The stack is empty!");
            return;
         end if;
         
         OprandStack.Pop_Discard(STACK);
         
      end if;
      
      if Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)) = "+" then
         if STATE = LOCKED then
            return;
         end if;
         
         if NumTokens /= 1 then
            INVALID := 1;
            Put_Line("Expect 1 arguments.");
            return;
         end if;
         
         if OprandStack.Size(STACK) < 2 then
            Put_Line("Add from insufficient stack!");
            return;
         end if;
         
         declare
            I : Integer;
            J : Integer;
         begin
            OprandStack.Pop(STACK, I);
            OprandStack.Pop(STACK, J);
            if not ((I < 0 and then J >= Integer'First - I) or 
              (I >= 0 and then J <= Integer'Last - I)) then
               Put_Line("Overflow will occur when doing addition!");
               INVALID := 1;
               return;
            end if;
            OprandStack.Push(STACK, I + J);
         end;
         
      end if;
      
      if Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)) = "-" then
         if STATE = LOCKED then
            return;
         end if;
         
         if NumTokens /= 1 then
            INVALID := 1;
            Put_Line("Expect 1 arguments.");
            return;
         end if;
         
         if OprandStack.Size(STACK) < 2 then
            Put_Line("Sub from insufficient stack!");
            return;
         end if;
         
         declare
            I : Integer;
            J : Integer;
         begin
            OprandStack.Pop(STACK, I);
            OprandStack.Pop(STACK, J);
            if not ((J > 0 and then I >= Integer'First + J) or 
              (J <= 0 and then I <= Integer'Last + J)) then
               Put_Line("Overflow will occur when doing subtraction!");
               INVALID := 1;
               return;
            end if;
            OprandStack.Push(STACK, I - J);
         end;
         
      end if;
      
      if Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)) = "*" then
         if STATE = LOCKED then
            return;
         end if;
         
         if NumTokens /= 1 then
            INVALID := 1;
            Put_Line("Expect 1 arguments.");
            return;
         end if;
         
         if OprandStack.Size(STACK) < 2 then
            Put_Line("Mul from insufficient stack!");
            return;
         end if;
         
         declare
            I : Integer;
            J : Integer;
         begin
            OprandStack.Pop(STACK, I);
            OprandStack.Pop(STACK, J);
            -- (Integer'First / (-1)) will overflow
            -- (Integer'First * (-1)) will overflow
            if ((I > 0 and J > 0) and then I > Integer'Last / J) then
               Put_Line("Overflow will occur when doing multiplication!");
               INVALID := 1;
               return;
            end if;
            if ((I < 0 and J < 0) and then I < Integer'Last / J) then
               Put_Line("Overflow will occur when doing multiplication!");
               INVALID := 1;
               return;
            end if;
            if ((I < 0 and J > 0) and then I < Integer'First / J) then
               Put_Line("Overflow will occur when doing multiplication!");
               INVALID := 1;
               return;
            end if;
            if ((I > 0 and J < 0) and then J < Integer'First / I) then
               Put_Line("Overflow will occur when doing multiplication!");
               INVALID := 1;
               return;
            end if;
            
            OprandStack.Push(STACK, I * J);
         end;
         
      end if;
      
      if Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)) = "/" then
         if STATE = LOCKED then
            return;
         end if;
         
         if NumTokens /= 1 then
            INVALID := 1;
            Put_Line("Expect 1 arguments.");
            return;
         end if;
         
         if OprandStack.Size(STACK) < 2 then
            Put_Line("Div from insufficient stack!");
            INVALID := 1;
            return;
         end if;
         
         declare
            I : Integer;
            J : Integer;
         begin
            OprandStack.Pop(STACK, I);
            OprandStack.Pop(STACK, J);
            
            if J = 0 then
               Put_Line("Cannot divide by zero");
               INVALID := 1;
               return;
            end if;
            
            -- (Integer'First / (-1)) will overflow
            if I = Integer'First and J = -1 then
               Put_Line("Overflow will occur when doing division!");
               INVALID := 1;
               return;
            end if;
            
            OprandStack.Push(STACK, (I / J));
         end;
      end if;
      
      -- For test only
      if Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)) = "print" then
         declare
            I: Integer;
         begin
            if OprandStack.Size(STACK) = 0 then
               return;
            end if;
            OprandStack.Pop(STACK, I);
            Put(I);New_Line;
            OprandStack.Push(STACK, I);
         end;
         
      end if;
      
   end Execute;
   
begin
   
   -- Put(MyCommandLine.Command_Name); Put_Line(" is running!");
   
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
   -- Put_Line("Finish setting the initialise master PIN");
   
   VariableStore.Init(VAR_DB);
   -- Put_Line("Finish Initialising var database");
   
   OprandStack.Init(STACK);
   -- Put_Line("Finish Initialising oprand stack");
   
   while True loop
      if STATE = 0 then
         Put("locked> ");
      else
         Put("unlocked> ");
      end if;
      
      Execute;
      
      if INVALID = 1 then
         return;
      end if;
      
      
   end loop;
   
end main2;
