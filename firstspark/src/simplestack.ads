generic
   Max_Size : Positive;
   type Item is private;
   Default_Item : Item;


package SimpleStack with SPARK_Mode is

   type SimpleStack is private;

   function Size(S: in SimpleStack) return Integer;

   procedure Init(S : out SimpleStack);

   procedure Push(S : in out SimpleStack; I : in Item) with
     Pre => (Size(S) /= Max_Size);

   procedure Pop(S : in out SimpleStack; I : out Item) with
     Pre => (Size(S) /= 0);

   private
   -- define SimpleStack type here
   type StorageArray is array(Integer range 1..Max_Size) of Item;
   type SimpleStack is record
      storage : StorageArray;
      size    : Integer range 0..Max_Size;
   end record;

   function Size(S: in SimpleStack) return Integer is
      (S.size);

end SimpleStack;
