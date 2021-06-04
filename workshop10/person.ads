package Person with SPARK_Mode is
   type Years is range 0..150;

   type NamePtr is not null access String;

   type Person is record
      Name : NamePtr;
      Age : Years;
   end record;

   procedure Print(P: in  Person);

   -- Swap_Name doesn't have this problem because it does not create an alias:
   -- it just swaps the two pointers.
   procedure Swap_Name(P : in out Person; S : in out NamePtr) with
     Post => (P.Name = S'Old and S = P'Old.Name and P.Age = P'Old.Age);

   -- The original Get_Name is problematic because it creates an alias S for P.Name.
   -- That means we are no long allowed to use P.Name.
   -- So P is not fully usable, which is a problem for any code that calls this
   -- procedure: it can no longer use its own data!
   -- Therefore to fix this problem, we have to make sure the pointers don't
   -- alias. One way to do that is to allocate a new string for P.Name to point to.
    procedure Get_Name(P : in out Person; S : out NamePtr) with
     Post => (S = P'Old.Name and P.Age = P'Old.Age);

end Person;
