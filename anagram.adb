pragma Ada_2012;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
procedure Anagram is

   --  Simple string predicates

   function Empty (S : String) return Boolean is (S'Last < S'First);
   --  Return True iff S is empty;

   --  String constructors

   function "*" (Left : Natural; Right : Character) return String is
     (1 .. Left => Right);
   function Space (N : Natural) return String is (N * ' ');

   --  Index computations

   function Middle (S : String) return Natural is
     (if S'Last < S'First then S'First - 1
      else S'First + (S'Last - S'First) / 2);

   --  Select parts of strings

   --  The Head and Tail functions allow for easy iteration by recursion.
   --  Note that Head requires S to be non-empty. Because Tail has O (N) time
   --  complexity and iteration has O (N) recursion depth, this way of
   --  iteration results in quadratic complexity in both time and space.

   function Head (S : String) return Character is (S (S'First));

   function Tail (S : String) return String is
     (if S'Last <= S'First then "" else S (S'First + 1 .. S'Last));

   --  The single argument form of Left and Right allows for convenient
   --  splitting of strings in half. When used for recursive iteration, this
   --  limits recursion depth to O (log N), and total time and space complexity
   --  will be O (N log N). For odd length, Left returns the longer part.

   function Left (S : String) return String is (S (S'First .. Middle (S)));
   function Right (S : String) return String is (S (Middle (S) + 1 .. S'Last));

   pragma Assert (Left ("Hello") = "Hel" and Left ("World!") = "Wor");
   pragma Assert (Right ("Hello") = "lo" and Right ("World!") = "ld!");

   function Left (S : String; N : Natural) return String is
     (if S'Length < N then S & Space (N - S'Length)
      else S (S'First .. S'First - 1 + N));

   function "-" (Left, Right : Character) return Integer is
     (Character'Pos (Left) - Character'Pos (Right));

   function "+" (Left : Character; Right : Integer) return Character is
     (Character'Val (Character'Pos (Left) + Right));

   function Upcase (C : Character) return Character is
     (if C in 'a' .. 'z' then C + ('A' - 'a') else C);
   pragma Assert (Upcase ('a') = 'A');
   pragma Assert (Upcase ('A') = 'A');
   pragma Assert (Upcase ('3') = '3');

   function Upcase (S : String) return String is
     (if Empty (S) then S else Upcase (Head (S)) & Upcase (Tail (S)));
   pragma Assert (Upcase ("Hello, World!") = "HELLO, WORLD!");

   function Count (S : String; C : Character) return Natural is
     (if S'Last < S'First then 0
      else Boolean'Pos (Head (S) = C) + Count (Tail (S), C));

   function Has (S : String; C : Character) return Boolean is
      (not Empty (S) and then (Head (S) = C or else Has (Tail (S), C)));
   pragma Assert (Has ("*sha*mater", '*'));

   function Non_Blanks (S : String) return String is
     (if Empty (S) then ""
      elsif Head (S) in ' ' | '?' then Non_Blanks (Tail (S))
      else Head (S) & Non_Blanks (Tail (S)));

   function Middle (S : String) return Character is (S (Middle (S)));

   function Insert (S : String; C : Character) return String is
     (if Empty (S) then (S'First => C)
      elsif C >= Middle (S) then Left (S) & Insert (Right (S), C)
      else Insert (S (S'First .. Middle (S) - 1), C) & Middle (S) & Right (S));
   pragma Assert (Insert ("Hllo", 'e') = "Hello");

   function Sort (S : String) return String is
     (if Empty (S) then S else Insert (Sort (Tail (S)), Head (S)));
   pragma Assert (Sort ("HELLO WORLD") = " DEHLLLOORW");

   function Match (Word : String; Pattern : String) return Boolean is
     (if Pattern = "" then Word = ""
      elsif Word = "" then Pattern = "*"
      elsif Head (Pattern) = '*' then Match (Word, Tail (Pattern))
                                        or else Match (Tail (Word), Pattern)
      elsif Head (Pattern) not in Head (Word) | '?' then False
      else Match (Tail (Word), Tail (Pattern)));
   pragma Assert (Match ("Hello", "H?*o"));
   pragma Assert (Match ("snail", "*na*"));
   pragma Assert (not Match ("World", "W*?orld"));

   function Match_Rack
     (Word    : String;
      Letters : String;
      Blanks  : Natural := 0) return Boolean
   is
      (Empty (Word)
         or else (not Empty (Letters)
                  and then Head (Word) = Head (Letters)
                  and then Match_Rack (Tail (Word), Tail (Letters), Blanks))
         or else (not Empty (Letters)
                  and then Head (Word) > Head (Letters)
                  and then Match_Rack (Word, Tail (Letters), Blanks))
         or else (Blanks > 0
                  and then Match_Rack (Tail (Word), Letters, Blanks - 1)));
   pragma Assert (Match_Rack (Sort ("BOARD"), "ABCDER", 1));
   pragma Assert (not Match_Rack (Sort ("BOARD"), "ABCDE", 1));

   subtype Letter is Character range 'A' .. 'Z';

   function Score (C : Character) return Natural is
     (if not (C in Letter) then 0
      else (case Letter (C) is
            when 'A' | 'E' | 'I'
               | 'L' | 'N' | 'O'
               | 'R' | 'S' | 'T'
               | 'U'             =>  1,
            when 'D' | 'G'       =>  2,
            when 'B' | 'C' | 'P'
               | 'M'             =>  3,
            when 'F' | 'H' | 'V'
               | 'W' | 'Y'       =>  4,
            when 'K'             =>  5,
            when 'J' | 'X'       =>  8,
            when 'Q' | 'Z'       => 10));

   function Score (S : String) return Natural is
     (if Empty (S) then 0 else Score (Head (S)) + Score (Tail (S)));

   function Scrabble_Word (S : String) return Boolean is
     (S'Length in 1 .. 15 and then Match_Rack (Sort (S),
        9 * 'A' & 2 * 'B' & 2 * 'C' & 4 * 'D' & 12 * 'E'
      & 2 * 'F' & 3 * 'G' & 2 * 'H' & 9 * 'I' &  1 * 'J'
      & 1 * 'K' & 4 * 'L' & 2 * 'M' & 6 * 'N' &  8 * 'O'
      & 2 * 'P' & 1 * 'Q' & 6 * 'R' & 4 * 'S' &  6 * 'T'
      & 4 * 'U' & 2 * 'V' & 2 * 'W' & 1 * 'X' &  2 * 'Y' & 1 * 'Z', 2));

   function Image (N : Natural) return String is -- No leading blanks
     ((if N < 10 then "" else Image (N / 10)) & ('0' + N));

   Rack     : constant String := Upcase (Argument (1));
   pragma Debug (Put_Line ("Rack = " & Rack));

   Wildcard : constant Boolean := Has (Rack, '*');
   pragma Debug (Put_Line ("Wildcard = " & Wildcard'Img));

   Letters  : constant String := Sort (Non_Blanks (Rack));
   pragma Debug (Put_Line ("Letters = " & Letters));

   Blanks   : constant Natural := Count (Rack, ' ') + Count (Rack, '?');
   pragma Debug (Put_Line ("Blanks =" & Blanks'Img));

   Pattern  : constant String :=
     (if Argument_Count > 1 then Upcase (Argument (2)) else "*");
   pragma Debug (Put_Line ("Pattern = " & Pattern));

   function Filter (Word : String) return Boolean is
     (if Wildcard then Match (Word, Rack)
      elsif Word'Length - Blanks > Letters'Length then False
      else Match_Rack (Sort (Word), Letters, Blanks)
        and then Match (Word, Pattern));

   Input : File_Type;

begin
   begin
      Open (Input, In_File, "words.txt");
      Set_Input (Input);

   exception
      when Name_Error => null;
      when Use_Error =>
         Put_Line (Standard_Error, "error reading words.txt");
         Set_Exit_Status (Failure);
   end;

   while not End_Of_File loop
      declare
         Word : constant String := Get_Line;
      begin
         if Filter (Word) then
            Put_Line (Word & Score (Word)'Img);
         end if;
      end;
   end loop;
end Anagram;
