package body MyStringTokeniser with SPARK_Mode is


   procedure Tokenise(S : in String; Tokens : in out TokenArray; Count : out Natural) is
      Index : Positive;
      Extent : TokenExtent;
      OutIndex : Integer := Tokens'First;
   begin
      Count := 0;
      if (S'First > S'Last) then
         return;
      end if;
      Index := S'First;
      while OutIndex <= Tokens'Last and Index <= S'Last and Count < Tokens'Length loop

         -- This loop invariant can imply the postcondition when the loop is exited
         -- by "S'Last - Extent.Length < Index"
         -- if Tokeniser has found tokens already, then these already-found tokens
         -- should satistfy that:
         -- 1) their start-index is >= S'First
         -- 2) their length is > 0 (they should contain at least one non-space charactor)
         -- 3) their end-index is <= S'Last
         -- In conclusion, this loop invariant states that all Tokens found in the
         -- string S should be in the S's index range and not empty
         pragma Loop_Invariant
           (for all J in Tokens'First..OutIndex-1 =>
              (Tokens(J).Start >= S'First and
                   Tokens(J).Length > 0) and then
            Tokens(J).Length-1 <= S'Last - Tokens(J).Start);
                 -- use "and then" here to prevent overflow
                 -- (e.g. Tokens(J).Length = 0 then Tokens(J).Length - 1 will
                 -- overflow since Length is Natural)

         -- It will imply the postcondition of procedure Tokenise when the loop
         -- is exited by "OutIndex = Tokens'Last". At that moment, the guard
         -- is false because Count = Tokens'Length.
         -- It will ensure that OutIndex is >= Tokens'First and >= 0, because the
         -- range of TokenArray's index is Positive and Count is >= 0. This prevents
         -- overflow (when executing for all J in Tokens'First..OutIndex-1 and
         -- OutIndex = Integer'First) and array index out of bounds (when
         -- executing Tokens(OutIndex) := Extent; and OutIndex = 0)
         pragma Loop_Invariant (OutIndex = Tokens'First + Count);

         -- look for start of next token
         while (Index >= S'First and Index < S'Last) and then Is_Whitespace(S(Index)) loop
            Index := Index + 1;
         end loop;
         if (Index >= S'First and Index <= S'Last) and then not Is_Whitespace(S(Index)) then
            -- found a token
            Extent.Start := Index;
            Extent.Length := 0;

            -- look for end of this token
            while Positive'Last - Extent.Length >= Index and then (Index+Extent.Length >= S'First and Index+Extent.Length <= S'Last) and then not Is_Whitespace(S(Index+Extent.Length)) loop
               Extent.Length := Extent.Length + 1;
            end loop;

            Tokens(OutIndex) := Extent;
            Count := Count + 1;

            -- check for last possible token, avoids overflow when incrementing OutIndex
            if (OutIndex = Tokens'Last) then
               -- reach the maximum capability of the given TokenArray
               return;
            else
               OutIndex := OutIndex + 1;
            end if;

            -- check for end of string, avoids overflow when incrementing Index
            if S'Last - Extent.Length < Index then
               return;
            else
               Index := Index + Extent.Length;
            end if;
         end if;
      end loop;
   end Tokenise;

end MyStringTokeniser;
