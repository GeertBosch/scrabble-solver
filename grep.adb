pragma Ada_2012;
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Text_IO;             use Ada.Text_IO;

--  The following implements a program that filters its input against patterns
--  given as arguments on the command line. Lines on standard input are copied
--  to standard output if they match any of the patterns, see Match.

procedure Grep is
   procedure Filter (Line : String);
   --  Print Line if and only if it matches any of the command line arguments

   function Head (S : String) return Character is 
     (S (S'First)); -- with Pre => S'Length > 0;
   --  Returns the first character of the (non-empty) string S

   function Tail (S : String) return String is
     (if S'Length = 0 then "" else S (S'First + 1 .. S'Last));
   --  Returns all but the first character of the string S

   function Up (C : Character) return Character is
     (if C in 'a' .. 'z' then Character'Val (Character'Pos (C) - 32) else C);
   --  Returns the uppercase version of ASCII character C, or just C otherwise.

   function Match (Text : String; Pattern : String) return Boolean is
     (if Pattern = "" then Text = ""
      elsif Head (Pattern) = '*' then
        (Text /= "" and then Match (Tail (Text), Pattern))
        or else Match (Text, Tail (Pattern))
      else Text /= "" and then Up (Head (Pattern)) in Up (Head (Text)) | '?'
        and then Match (Tail (Text), Tail (Pattern)));
   --  Returns True if Text matches Pattern, ignoring case. A '?' character
   --  in the pattern matches any character in the input and a '*' chacacter
   --  matches any substring in the input, including the null string.

   function Match (Text : String) return Boolean is
     (for some Arg in 1 .. Argument_Count => Match (Text, Argument (Arg)));
   --  Returns True if Text matches any of the arguments

   procedure Filter (Line : String) is
   begin
      if Match (Line) then
         Put_Line (Line);
      end if;
   end Filter;
   
begin
   while not End_Of_File loop
      Filter (Get_Line);
   end loop;
end Grep;
