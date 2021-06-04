with Person; use Person;
with Ada.Text_IO; with Ada.Integer_Text_IO;

procedure Main with SPARK_Mode is
   S : Person.NamePtr := new String'("John Doe");
   Age : Person.Years := Person.Years(35);
   P : Person.Person := (Name => S,Age => Age);
   S2 : Person.NamePtr := new String'("Jane Doe");

   function Name_Compare(N1 : in Person.NamePtr; N2 : Person.NamePtr)
                         return Boolean
   is
      I : Integer := N1.all'First;
      J : Integer := N2.all'First;
   begin
      if N1.all'Length /= N2.all'Length then
         return False;
      end if;

      while I <= N1.all'Last and J <= N2.all'Last loop
         pragma Loop_Invariant (I >= N1.all'First and J >= N2.all'First);
         if N1.all(I) /= N2.all(J) then
            return False;
         else
            if (I < N1.all'Last) then
               I := I + 1;
            elsif (I = N1.all'Last) then
               return True;
            end if;
            if (J < N2.all'Last) then
               J := J + 1;
            end if;
         end if;
      end loop;
      return True;
   end Name_Compare;

begin
   Ada.Text_IO.Put_Line(S.all);

   Print(P);
   pragma Assert(P.Name.all = "John Doe" and P.Age = Age);
   pragma Assert(S2.all = "Jane Doe");


   Swap_Name(P,S2);

   pragma Assert (P.Name.all = "Jane Doe");
   pragma Assert (S2.all = "John Doe");

   Print(P);

   if Name_Compare(S2, S2) then
      Ada.Text_IO.Put_Line("Equal");
   else
      Ada.Text_IO.Put_Line("Not Equal");
   end if;

   Get_Name(P,S2);
   Ada.Text_IO.Put_Line(S2.all);
   pragma Assert (P.Age = Age);


end Main;
