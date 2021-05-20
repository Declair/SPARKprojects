pragma SPARK_Mode (On);

with VariableStore;
with MyCommandLine;
with OprandStack;
with PIN;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body calculator with SPARK_Mode is

   procedure Init(C : out calculator; Initial_PIN : in PIN.PIN) is
   begin
      C.STATE := LOCKED;
      VariableStore.Init(C.VAR_DB);
      OprandStack.Init(C.STACK);
      C.MASTER_PIN := Initial_PIN;
   end;

   procedure Do_Unlock(C : in out calculator; Provided_PIN : in PIN.PIN) is
   begin
      if PIN."="(C.MASTER_PIN, Provided_PIN) then
         C.STATE := UNLOCKED;
      end if;
   end;

   procedure Do_Lock(C : in out calculator; Provided_PIN : in PIN.PIN) is
   begin
      C.STATE := LOCKED;
      C.MASTER_PIN := Provided_PIN;
   end;

   procedure Do_Push(C : in out calculator; I : in Integer) is
   begin
      OprandStack.Push(C.STACK, I);
   end;

   procedure Do_Pop(C : in out calculator) is
   begin
      OprandStack.Pop_Discard(C.STACK);
   end;

   procedure Do_Load(C: in out calculator; Var : in VariableStore.Variable) is
      I : Integer;
   begin
      if VariableStore.Has_Variable(C.VAR_DB, Var) then
         I := VariableStore.Get(C.VAR_DB, Var);
         OprandStack.Push(C.STACK, I);
      end if;
   end;

   procedure Do_Store(C: in out calculator; Var : in VariableStore.Variable) is
      I : Integer;
   begin
      OprandStack.Pop(C.STACK, I);
      VariableStore.Put(C.VAR_DB, Var, I);
   end;

   procedure Do_Remove(C: in out calculator; Var : in VariableStore.Variable) is
   begin
      if VariableStore.Has_Variable(C.VAR_DB, Var) then
         VariableStore.Remove(C.VAR_DB, VAR);
      end if;
   end;

   procedure Do_Add(C: in out calculator; Will_Overflow : out Boolean) is
      I : Integer;
      J : Integer;
   begin
      OprandStack.Pop(C.STACK, I);
      OprandStack.Pop(C.STACK, J);
      if not ((I < 0 and then J >= Integer'First - I) or
                (I >= 0 and then J <= Integer'Last - I)) then
         Will_Overflow := True;
         return;
      end if;
      Will_Overflow := False;
      OprandStack.Push(C.STACK, I + J);
   end;

   procedure Do_Substract(C: in out calculator; Will_Overflow : out Boolean) is
      I : Integer;
      J : Integer;
   begin
      OprandStack.Pop(C.STACK, I);
      OprandStack.Pop(C.STACK, J);
      if not ((J > 0 and then I >= Integer'First + J) or
                (J <= 0 and then I <= Integer'Last + J)) then
         Will_Overflow := True;
         return;
      end if;
      Will_Overflow := False;
      OprandStack.Push(C.STACK, I - J);
   end;

   procedure Do_Multiply(C: in out calculator; Will_Overflow : out Boolean) is
      I : Integer;
      J : Integer;
   begin
      OprandStack.Pop(C.STACK, I);
      OprandStack.Pop(C.STACK, J);
      -- (Integer'First / (-1)) will overflow
      -- (Integer'First * (-1)) will overflow
      if ((I > 0 and J > 0) and then (I > Integer'Last / J or J > Integer'Last / I)) then
         Will_Overflow := True;
         return;
      end if;
      if ((I < 0 and J < 0) and then (I < Integer'Last / J or J < Integer'Last / I)) then
         Will_Overflow := True;
         return;
      end if;
      if ((I < 0 and J > 0) and then I < Integer'First / J) then
         Will_Overflow := True;
         return;
      end if;
      if ((I > 0 and J < 0) and then J < Integer'First / I) then
         Will_Overflow := True;
         return;
      end if;

      --pragma Assert (if I > 0 and J < 0 then I * J >= Integer'First);
      --pragma Assert (if I < 0 and J > 0 then I * J >= Integer'First);
      --pragma Assert (if I > 0 and J > 0 then I * J <= Integer'Last);
      --pragma Assert (if I < 0 and J < 0 then I * J <= Integer'Last);
      Will_Overflow := False;
      OprandStack.Push(C.STACK, I * J);
   end;

   procedure Do_Divide(C: in out calculator; Will_Overflow : out Boolean) is
      I : Integer;
      J : Integer;
   begin
      OprandStack.Pop(C.STACK, I);
      OprandStack.Pop(C.STACK, J);

      if J = 0 then
         Will_Overflow := True;
         return;
      end if;

      -- (Integer'First / (-1)) will overflow
      if I = Integer'First and J = -1 then
         Will_Overflow := True;
         return;
      end if;

      pragma Assert (if I = 0 then I / J = 0);
      Will_Overflow :=  False;
      OprandStack.Push(C.STACK, (I / J));
   end;


end calculator;
