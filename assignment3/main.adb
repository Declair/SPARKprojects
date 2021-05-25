-- Names:
-- 1061668 Jihao Deng
-- 1054053 Qiqi Hu

-- Task 4:
-- In our implementation, performing each operations is written in a procedure
-- in calculator package, such as Do_Add, Do_Push.

-- 1. The arithmetic operations, push, pop, load, store, remove, and lock can
-- only ever be performed when the calculator is in the unlocked state:
-- The preconditions of these operations' procedures say that "C.STATE = UNLOCKED", 
-- which means these operations can only be performed in unlocked state. 

-- 2. The Unlock operation can only ever be performed when the calculator is in 
-- the locked state. And when it's performed, it will not change the master PIN:
-- The precondition of procedure Do_Unlock in calculator.ads "C.STATE = LOCKED". 
-- It means unlock operation can only be performed in locked state. And it also
-- said "PIN."="(C.MASTER_PIN, C.MASTER_PIN'Old)", meaning that the master PIN 
-- will not change.

-- 3. The Lock operation, when it is performed, should update the master PIN 
-- with the new PIN:
-- The postcondition of Do_Lock wrote "PIN."="(C.MASTER_PIN, Provided_PIN)", saying
-- that the new PIN equals to the supplied new PIN after Lock operation is performed.

-- 4. The Lock operation, when it is performed, will change the state into Locked:
-- The postcondition of Do_Lock "C.STATE = LOCKED", saying that the after this 
-- procedure is performed, the state will be locked.

-- 5. After arithmetic operations, push, pop, load, store and remove operations
-- are performed, the calculator state will remain unlocked, and PIN won't change:
-- The postconditions of these procedures states that "C.STATE = UNLOCKED" and
-- "PIN."="(C.MASTER_PIN, C.MASTER_PIN'Old)" which ensure the state and PIN of 
-- calculator will not be changed.

-- 6. The Unlock operation will only change the state into unlocked when the PIN
-- is correct:
-- The postcondition of Do_Unlock checks if the PINs are the same. If so, the 
-- state will be unlocked; if not, the state will be locked.

-- 7. When the program is started, the calculator will be locked and the PIN is
-- the initial given PIN.
-- The postcondition of procedure Init in calculator.ads checks that the state
-- of the calculator is locked and the PIN is equals to the given PIN.

-- 8. Behaviours like oprand stack overflow will never happen.
-- The precondition of Do_Load, Do_Store, Do_Push, Do_Pop will check the size
-- of oprand stack and variable database.

pragma SPARK_Mode (On);

with StringToInteger;
with VariableStore;
with MyCommandLine;
with MyString;
with MyStringTokeniser;
with PIN;
with OprandStack;
with calculator;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Long_Long_Integer_Text_IO;

with Ada.Containers; use Ada.Containers;  -- to check if variable store is full


procedure main is
   MyCalculator : calculator.calculator;
   MASTER_PIN  : PIN.PIN ;
   package Lines is new MyString(Max_MyString_Length => 2048);
   T : MyStringTokeniser.TokenArray(1..5) := (others => (Start => 1, Length => 0));
   INVALID : Integer := 0;
   
   procedure Execute is
      S  : Lines.MyString;
      NumTokens : Natural;
   begin
      Lines.Get_Line(S);
      MyStringTokeniser.Tokenise(Lines.To_String(S),T,NumTokens);
      -- Put("You entered "); Put(NumTokens); Put_Line(" tokens.");
      
      if NumTokens = 0 then
         -- blank line, ignore
         return;
      end if;
      
      if Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)) = "unlock" then
         if calculator.Current_State(MyCalculator) = calculator.UNLOCKED then
            return;
         end if;
         
         if NumTokens /= 2 then
            INVALID := 1;
            Put("Expect 2 arguments but instead found ");Put_Line(NumTokens'Image);
            return;
         end if;
         
         declare
            Str : String := Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1));
         begin
            if not (Str'Length = 4 and
              (for all I in Str'Range => Str(I) >= '0' and Str(I) <= '9')) then
               INVALID := 1;
               Put_Line("Invalid PIN, it must be 4-digit strings between 0000 and 9999");
               return;
            end if;
            
            if calculator.Current_State(MyCalculator) = calculator.LOCKED then
               calculator.Do_Unlock(MyCalculator, PIN.From_String(Str));
            end if;
            
         end;
         return; 
      end if;
      
      if Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)) = "lock" then
         if calculator.Current_State(MyCalculator) = calculator.LOCKED then
            return;
         end if;
         
         if NumTokens /= 2 then
            INVALID := 1;
            Put("Expect 2 arguments but instead found ");Put_Line(NumTokens'Image);
            return;
         end if;
         
         declare
            Str : String := Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1));
         begin
            if not (Str'Length = 4 and
              (for all I in Str'Range => Str(I) >= '0' and Str(I) <= '9')) then
               INVALID := 1;
               Put_Line("Invalid PIN");
               return;
            end if;
            if calculator.Current_State(MyCalculator) = calculator.UNLOCKED then
               calculator.Do_Lock(MyCalculator, PIN.From_String(Str));
            end if;
         end;
         return;
      end if;
      
      if Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)) = "load" then
         if calculator.Current_State(MyCalculator) = calculator.LOCKED then
            return;
         end if;
         
         if NumTokens /= 2 then
            INVALID := 1;
            Put("Expect 2 arguments but instead found ");Put_Line(NumTokens'Image);
            return;
         end if;
         
         declare
            Str : String := Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1));
         begin
            if Str'Length > VariableStore.Max_Variable_Length then
               Put_Line("The length of the variable is too long!");
               return;
            end if;
            
            if OprandStack.Size(MyCalculator.STACK) >= OprandStack.Capability then
               Put_Line("The stack is full!");
               return;
            end if;
            
            if calculator.Current_State(MyCalculator) = calculator.UNLOCKED then
               declare
                  VAR : VariableStore.Variable := VariableStore.From_String(Str);
               begin
                  calculator.Do_Load(MyCalculator, VAR);
               end;
            end if;         
         end;
         return;
      end if;
      
      if Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)) = "store" then
         if calculator.Current_State(MyCalculator) = calculator.LOCKED then
            return;
         end if;
         
         if NumTokens /= 2 then
            INVALID := 1;
            Put("Expect 2 arguments but instead found ");Put_Line(NumTokens'Image);
            return;
         end if;
         
         declare
            Str : String := Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1));
         begin
            if OprandStack.Size(MyCalculator.STACK) = 0 then
               Put_Line("The stack is empty!");
               return;
            end if;
         
            if Str'Length > VariableStore.Max_Variable_Length then
               Put_Line("The length of the variable is too long!");
               return;
            end if;
            
            declare
               VAR : VariableStore.Variable := VariableStore.From_String(Str);
            begin
               if not (VariableStore.Length(MyCalculator.VAR_DB) < VariableStore.Max_Entries or
                      VariableStore.Has_Variable(MyCalculator.VAR_DB, VAR)) then
                  Put_Line("The variable database is full.");
                  return;
               end if;
               if calculator.Current_State(MyCalculator) = calculator.UNLOCKED then
                  calculator.Do_Store(MyCalculator, VAR);
               end if;
            end;
            
         end;
         return;
      end if;
      
      if Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)) = "remove" then
         if calculator.Current_State(MyCalculator) = calculator.LOCKED then
            return;
         end if;
         
         if NumTokens /= 2 then
            INVALID := 1;
            Put("Expect 2 arguments but instead found ");Put_Line(NumTokens'Image);
            return;
         end if;
         
         declare
            Str : String := Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1));
         begin
            if Str'Length > VariableStore.Max_Variable_Length then
               Put_Line("The length of the variable is too long!");
               return;
            end if;
            
            if calculator.Current_State(MyCalculator) = calculator.UNLOCKED then
               declare
                  VAR : VariableStore.Variable := VariableStore.From_String(Str);
               begin
                  calculator.Do_Remove(MyCalculator, VAR);
               end;
            end if;         
         end;
         return;
      end if;
      
      if Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)) = "list" then
         if calculator.Current_State(MyCalculator) = calculator.LOCKED then
            return;
         end if;
         
         if NumTokens /= 1 then
            INVALID := 1;
            Put("Expect 1 argument but instead found ");Put_Line(NumTokens'Image);
            return;
         end if;
         if calculator.Current_State(MyCalculator) = calculator.UNLOCKED then
            VariableStore.Print(MyCalculator.VAR_DB);
         end if;
         return;
      end if;
      
      if Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)) = "push" then
         if calculator.Current_State(MyCalculator) = calculator.LOCKED then
            return;
         end if;
         
         if NumTokens /= 2 then
            INVALID := 1;
            Put("Expect 2 arguments but instead found ");Put_Line(NumTokens'Image);
            return;
         end if;
         
         if OprandStack.Size(MyCalculator.STACK) >= OprandStack.Capability then
            Put_Line("The stack is full!");
            return;
         end if;
         
         if calculator.Current_State(MyCalculator) = calculator.UNLOCKED then
            declare
               Str : String := Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1));
            begin
               calculator.Do_Push(MyCalculator, StringToInteger.From_String(Str));
            end;
         end if;
         return;
      end if;
      
      if Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)) = "pop" then
         if calculator.Current_State(MyCalculator) = calculator.LOCKED then
            return;
         end if;
         
         if NumTokens /= 1 then
            INVALID := 1;
            Put("Expect 1 argument but instead found ");Put_Line(NumTokens'Image);
            return;
         end if;
         
         if OprandStack.Size(MyCalculator.STACK) = 0 then
            Put_Line("The stack is empty!");
            return;
         end if;
         
         if calculator.Current_State(MyCalculator) = calculator.UNLOCKED then
            calculator.Do_Pop(MyCalculator);
         end if;
         return;
      end if;
      
      if Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)) = "+" then
         if calculator.Current_State(MyCalculator) = calculator.LOCKED then
            return;
         end if;
         
         if NumTokens /= 1 then
            INVALID := 1;
            Put("Expect 2 argument but instead found ");Put_Line(NumTokens'Image);
            return;
         end if;
         
         if OprandStack.Size(MyCalculator.STACK) < 2 then
            Put_Line("Add from insufficient stack!");
            return;
         end if;
         
         if calculator.Current_State(MyCalculator) = calculator.UNLOCKED then
            declare
               Will_Overflow : Boolean;
            begin
               calculator.Do_Add(MyCalculator, Will_Overflow);
               if Will_Overflow then
                  INVALID := 1;
                  Put_Line("Overflow will occur when doing addition!");
                  return;
               end if;
            end;
         end if;
         return;
      end if;
      
      if Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)) = "-" then
         if calculator.Current_State(MyCalculator) = calculator.LOCKED then
            return;
         end if;
         
         if NumTokens /= 1 then
            INVALID := 1;
            Put("Expect 1 argument but instead found ");Put_Line(NumTokens'Image);
            return;
         end if;
         
         if OprandStack.Size(MyCalculator.STACK) < 2 then
            Put_Line("Sub from insufficient stack!");
            return;
         end if;
         
         if calculator.Current_State(MyCalculator) = calculator.UNLOCKED then
            declare
               Will_Overflow : Boolean;
            begin
               calculator.Do_Substract(MyCalculator, Will_Overflow);
               if Will_Overflow then
                  INVALID := 1;
                  Put_Line("Overflow will occur when doing subtraction!");
                  return;
               end if;
            end;
         end if;
         return;
      end if;
      
      if Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)) = "*" then
         if calculator.Current_State(MyCalculator) = calculator.LOCKED then
            return;
         end if;
         
         if NumTokens /= 1 then
            INVALID := 1;
            Put("Expect 1 argument but instead found ");Put_Line(NumTokens'Image);
            return;
         end if;
         
         if OprandStack.Size(MyCalculator.STACK) < 2 then
            Put_Line("Mul from insufficient stack!");
            return;
         end if;
         
         if calculator.Current_State(MyCalculator) = calculator.UNLOCKED then
            declare
               Will_Overflow : Boolean;
            begin
               calculator.Do_Multiply(MyCalculator, Will_Overflow);
               if Will_Overflow then
                  INVALID := 1;
                  Put_Line("Overflow will occur when doing multiplication!");
                  return;
               end if;
            end;
         end if;
         return;
      end if;
      
      if Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1)) = "/" then
         if calculator.Current_State(MyCalculator) = calculator.LOCKED then
            return;
         end if;
         
         if NumTokens /= 1 then
            INVALID := 1;
            Put("Expect 1 argument but instead found ");Put_Line(NumTokens'Image);
            return;
         end if;
         
         if OprandStack.Size(MyCalculator.STACK) < 2 then
            Put_Line("Div from insufficient stack!");
            return;
         end if;
         
         if calculator.Current_State(MyCalculator) = calculator.UNLOCKED then
            declare
               Will_Overflow : Boolean;
            begin
               calculator.Do_Divide(MyCalculator, Will_Overflow);
               if Will_Overflow then
                  INVALID := 1;
                  Put_Line("Dividing by zero or Overflow will occur when doing division!");
                  return;
               end if;
            end;
         end if;
         return;
      end if;
      
      -- if reach here, the program must have received an invalid command
      Put_Line("Unrecognised command.");
      INVALID := 1;
      
   end Execute;
   
begin
   -- Put(MyCommandLine.Command_Name); Put_Line(" is running!");
   
   -- Receiving and setting master PIN from commandline argument
   if MyCommandLine.Argument_Count = 0 then
      Put_Line("No initial master PIN was provided.");
      return;
   elsif MyCommandLine.Argument_Count = 1 then
      
      if MyCommandLine.Argument(1)' Length = 4 and
        (for all I in MyCommandLine.Argument(1)'Range => 
             MyCommandLine.Argument(1)(I) >= '0' and 
             MyCommandLine.Argument(1)(I) <= '9') then
         MASTER_PIN := PIN.From_String(MyCommandLine.Argument(1));
      else
         Put_Line("Invalid initial master PIN.");
         return;
      end if;
   else
      Put_Line("The program was started with too many arguments");
      return;
   end if;
   
   -- Initialising the calculater, including setting initial master PIN, 
   -- creating variable store database and stack
   calculator.Init(MyCalculator, MASTER_PIN);
   
   while True loop
      if calculator.Current_State(MyCalculator) = calculator.LOCKED then
         Put("locked> ");
      else
         Put("unlocked> ");
      end if;
      
      Execute;
      
      if INVALID = 1 then
         -- recieved invalid input, exit the Main procedure
         return;
      end if;
      
   end loop;
   
end main;
