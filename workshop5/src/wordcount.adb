with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Integer_Text_IO;
use  Ada.Integer_Text_IO;

with ProtectedStack;
with Ada.Strings.Unbounded;

with Ada.Characters.Latin_1;

-- The WordCount program counts the number of characters and words in
--  a string received from standard input. A word is any alphanum
--  character separated by a space or tab
procedure WordCount is

   package ASU renames Ada.Strings.Unbounded;
   use ASU;
   package StringStack is new ProtectedStack(100, ASU.Unbounded_String);

   Ch        : Character;            -- the current character
   Word      : ASU.Unbounded_String; -- the current word

   -- The number of characters and words
   NumChars : Integer := 0;
   NumWords : Integer := 0;
   Flag     : Integer := 0;

   -- a stack for putting words into
   St : StringStack.Stack;

begin

   Get(Ch);
   Word := ASU.To_Unbounded_String("");

   -- the input is terminated by a '#'
   while (Ch /= '#') loop

      NumChars := NumChars + 1;

      -- Ada.Characters.Latin_1.HT is 'tab'
      if Ch = ' ' or Ch = Ada.Characters.Latin_1.HT then
         if Flag = 1 then
            -- end of a word
            St.Push(Word);
            NumWords := NumWords + 1;
            Word := ASU.To_Unbounded_String("");
         end if;
         Flag := 0;

      else
         Flag := 1;
         Word := Word & Ch;
      end if;

      Get(Ch);

   end loop;

   -- push the terminating word
   if Flag = 1 then
      St.Push(Word);
      NumWords := NumWords + 1;
   end if;

   Put("Number of Words     : ");
   Ada.Integer_Text_IO.Put(NumWords);
   New_Line;
   Put("Number of Characters: ");
   Ada.Integer_Text_IO.Put(NumChars);
   New_Line;
   Put("Words in reverse order:"); New_Line;
   while NumWords > 0 loop
      St.Pop(Word);
      Put(ASU.To_String(Word) & ' ');
      NumWords := NumWords - 1;
   end loop;

end WordCount;
