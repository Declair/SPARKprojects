with Ada.Text_IO; with Ada.Integer_Text_IO;

package body Person with SPARK_Mode is

   procedure Swap_Name(P : in out Person; S : in out NamePtr) is
      Temp : NamePtr := P.Name;
   begin
      P.Name := S;
      S := Temp;
   end Swap_Name;

   procedure Get_Name(P : in out Person; S : out NamePtr) is
   begin
      S := P.Name;
      P.Name := new String'(S.all); -- allocate a new string for P.Name to point to.
   end Get_Name;

   procedure Print(P: in Person) is
   begin
      Ada.Text_IO.Put("Person: ");
      Ada.Text_IO.Put(P.Name.all);
      Ada.Text_IO.Put(" is ");
      Ada.Integer_Text_IO.Put(Item => Integer(P.Age), Width => 0);
      Ada.Text_IO.Put(" years old.");
      Ada.Text_IO.New_Line;
   end Print;

end Person;
