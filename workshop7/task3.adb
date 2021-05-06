package body Task3 with
SPARK_Mode => On
is

   -- if run Examine file, then no warning messages shows up
   -- because Examine only checks flow
   --         Prove checks everything

   procedure Task3Procedure(Input : in Integer; Result : out Integer)
   is
   begin
      --  if Input * 10 <= Integer'Last and
      --    Input * 10 >= Integer'First then
      --     Result := Input * 10;
      --  else
      --     Result := Input;
      --  end if;

      -- corrected program
      if Input < Integer'Last / 10 and
        Input >= Integer'First / 10 then
         Result := Input * 10;
      else
         Result := Input;
      end if;
   end Task3Procedure;
end Task3;

