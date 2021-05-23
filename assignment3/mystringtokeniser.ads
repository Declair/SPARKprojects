with Ada.Characters.Latin_1;

package MyStringTokeniser with SPARK_Mode is

   type TokenExtent is record
      Start : Positive;
      Length : Natural;
   end record;

   type TokenArray is array(Positive range <>) of TokenExtent;

   function Is_Whitespace(Ch : Character) return Boolean is
     (Ch = ' ' or Ch = Ada.Characters.Latin_1.LF or
        Ch = Ada.Characters.Latin_1.HT);

   -- In the postcondition of this procedure:
   -- "Count <= Tokens'Length" specifies that the number of recognised tokens will
   -- not exceed the capability of the given TokenArray. It ensures that there no
   -- index out of range will occur;
   --
   -- "Tokens(Index).Start >= S'First" requires each recognised token is within
   -- S's index range. Without it, precondition "To <= Length(M)" from Substring
   -- in mystring.ads might fail, because To was computed by From + Length - 1.
   --
   -- "Tokens(Index).Length > 0" requires each recognised token is not empty.
   -- Without it, precondition "From <= To" from Substring in mystring.ads
   -- might fail.
   --
   -- "Tokens(Index).Length-1 <= S'Last - Tokens(Index).Start" limits the length
   -- of each recognised token. Their length is not larger than S's last index
   -- minus their start-index. Without this condition, overflow might happen
   -- because no the upper bound of Length is defined.
   procedure Tokenise(S : in String; Tokens : in out TokenArray; Count : out Natural) with
     Pre => (if S'Length > 0 then S'First <= S'Last) and Tokens'First <= Tokens'Last,
     Post => Count <= Tokens'Length and
     (for all Index in Tokens'First..Tokens'First+(Count-1) =>
          (Tokens(Index).Start >= S'First and
          Tokens(Index).Length > 0) and then
            Tokens(Index).Length-1 <= S'Last - Tokens(Index).Start);


end MyStringTokeniser;
