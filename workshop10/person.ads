package Person with SPARK_Mode is
   type Years is range 0..150;

   type NamePtr is not null access String;

   type Person is record
      Name : NamePtr;
      Age : Years;
   end record;

   procedure Print(P: in  Person);

   procedure Swap_Name(P : in out Person; S : in out NamePtr) with
     Post => (P.Name = S'Old and S = P'Old.Name and P.Age = P'Old.Age);

   procedure Get_Name(P : in out Person; S : out NamePtr) with
     Post => (S = P'Old.Name and P.Age = P'Old.Age);

end Person;
