with VariableStore;
with SimpleStack;
with OprandStack;
with PIN;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Long_Long_Integer_Text_IO;

with Ada.Containers; use Ada.Containers;  -- to check if variable store is full


package calculator with SPARK_Mode is
   
   LOCKED : constant Integer := 0;
   UNLOCKED : constant Integer := 1;
   
   type calculator is record
      VAR_DB : VariableStore.Database;
      MASTER_PIN  : PIN.PIN ;      
      STACK : OprandStack.SimpleStack;
      STATE : Integer;
   end record;
   
   function VariableDatabase_Full(C : in calculator) return Boolean;
      
   function OprandStack_Capability return Integer;
   
   function OprandStack_Size(C : in calculator) return Integer;
   
   function Current_State(C : in calculator) return Integer;
   
   procedure Init(C : out calculator; Initial_PIN : in PIN.PIN) with
     Post => (C.STATE = LOCKED and PIN."="(C.MASTER_PIN, Initial_PIN));
   
   procedure Do_Unlock(C : in out calculator; Provided_PIN : in PIN.PIN) with
     Pre => (C.STATE = LOCKED), 
     Post => (if PIN."="(C.MASTER_PIN, Provided_PIN) then C.STATE = UNLOCKED
             else C.STATE = LOCKED);
   
   procedure Do_Lock(C : in out calculator; Provided_PIN : in PIN.PIN) with
     Pre => (C.STATE = UNLOCKED),
     Post => (C.STATE = LOCKED and PIN."="(C.MASTER_PIN, Provided_PIN));
   
   procedure Do_Push(C : in out calculator; I : in Integer) with
     Pre => (C.STATE = UNLOCKED and OprandStack_Size(C) < OprandStack_Capability),
     Post => (C.STATE = UNLOCKED);
   
   procedure Do_Pop(C : in out calculator) with
     Pre => (C.STATE = UNLOCKED and OprandStack_Size(C) > 0),
     Post => (C.STATE = UNLOCKED);
   
   procedure Do_Load(C: in out calculator; Var : in VariableStore.Variable) with
     Pre => (C.STATE = UNLOCKED and OprandStack_Size(C) < OprandStack_Capability),
     Post => (C.STATE = UNLOCKED);
   
   procedure Do_Store(C: in out calculator; Var : in VariableStore.Variable) with
     Pre => (C.STATE = UNLOCKED and OprandStack_Size(C) > 0 and
                 (VariableStore.Length(C.VAR_DB) < VariableStore.Max_Entries or 
                        VariableStore.Has_Variable(C.VAR_DB, Var))),
     Post => (C.STATE = UNLOCKED);
   
   procedure Do_Remove(C: in out calculator; Var : in VariableStore.Variable) with
     Pre => (C.STATE = UNLOCKED),
     Post => (C.STATE = UNLOCKED);
   
   procedure Do_Add(C: in out calculator; Will_Overflow : out Boolean) with
     Pre => (C.STATE = UNLOCKED and OprandStack_Size(C) >= 2),
     Post => (C.STATE = UNLOCKED);
   
   procedure Do_Substract(C: in out calculator; Will_Overflow : out Boolean) with
     Pre => (C.STATE = UNLOCKED and OprandStack_Size(C) >= 2),
     Post => (C.STATE = UNLOCKED);
   
   procedure Do_Multiply(C: in out calculator; Will_Overflow : out Boolean) with
     Pre => (C.STATE = UNLOCKED and OprandStack_Size(C) >= 2),
     Post => (C.STATE = UNLOCKED);
   
   procedure Do_Divide(C: in out calculator; Will_Overflow : out Boolean) with
     Pre => (C.STATE = UNLOCKED and OprandStack_Size(C) >= 2),
     Post => (C.STATE = UNLOCKED);
   
   function VariableDatabase_Full(C : in calculator) return Boolean is
     (VariableStore.Length(C.VAR_DB) >= VariableStore.Max_Entries);
   
   function OprandStack_Capability return Integer is
     (OprandStack.Capability);
   
   function OprandStack_Size(C : in calculator) return Integer is
     (OprandStack.Size(C.STACK));
   
   function Current_State(C : in calculator) return Integer is
     (C.STATE);

end calculator;
