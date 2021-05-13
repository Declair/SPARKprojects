with GNAT.OS_Lib;
package body SimpleStack with SPARK_Mode is

   procedure Init(S : out SimpleStack) is
   begin
      S.size := 0;
      S.storage := (others => Default_Item);
   end Init;

   procedure Push(S : in out SimpleStack; I : in Item) is
   begin
      S.size := S.size + 1;
      S.storage(S.size) := I;
   end Push;

   procedure Pop(S : in out SimpleStack; I : out Item) is
   begin
      if S.size = 0 then
         GNAT.OS_Lib.OS_Exit (0);
      end if;
      I := S.storage(S.size);
      S.size := S.size - 1;
   end Pop;

end SimpleStack;
