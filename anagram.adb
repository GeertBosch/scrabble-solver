pragma Ada_2012;
with Ada.Text_IO; use Ada;
with Ada.Command_Line; use Ada.Command_Line;
with Textfun; use Textfun;

procedure Anagram is

   Board_Size : constant := 15;

   Tiles : constant String :=
      9 * 'A' & 2 * 'B' & 2 * 'C' & 4 * 'D' & 12 * 'E' & 2 * 'F' & 3 * 'G'
    & 2 * 'H' & 9 * 'I' & 1 * 'J' & 1 * 'K' & 4 * 'L' & 2 * 'M' & 6 * 'N' 
    & 8 * 'O' & 2 * 'P' & 1 * 'Q' & 6 * 'R' & 4 * 'S' & 6 * 'T' & 4 * 'U'
    & 2 * 'V' & 2 * 'W' & 1 * 'X' & 2 * 'Y' & 1 * 'Z';

   subtype Board_String is String (1 .. Board_Size**2);

   Error_Exit : exception;

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

   function "-" (Left, Right : String) return String is
     (if Left = "" or else Right = "" then Left
      elsif Head (Left) = Head (Right) then Tail (Left) - Tail (Right)
      else Head (Left) & (Tail (Left) - Right));

   function Score (C : Character) return Natural is
     (if not (C in Letter) then 0
      else (case Letter (C) is
            when 'A' | 'E' | 'I' | 'L' | 'N' 
               | 'O' | 'R' | 'S' | 'T' | 'U' =>  1,
            when 'D' | 'G'                   =>  2,
            when 'B' | 'C' | 'P' | 'M'       =>  3,
            when 'F' | 'H' | 'V' | 'W' | 'Y' =>  4,
            when 'K'                         =>  5,
            when 'J' | 'X'                   =>  8,
            when 'Q' | 'Z'                   => 10));

   function Score (S : String) return Natural is
     (if Empty (S) then 0 else Score (Head (S)) + Score (Tail (S)));

   function Scrabble_Word (S : String) return Boolean is
     (S'Length in 1 .. 15 and then Match_Rack (Sort (S), Tiles));

   function Image (N : Natural) return String is -- No leading blanks
     ((if N < 10 then "" else Image (N / 10)) & ('0' + N));

   function Get_Letters (S : String) return String is
     (if Empty (S) then "" 
      elsif Head (S) not in Letter then Get_Letters (Tail (S))
      else Head (S) & Get_Letters (Tail (S)));

   Rack     : constant String := 
     (if Argument_Count > 0 then Upcase (Argument (1)) else "");
   Pattern  : constant String :=
     (if Argument_Count > 1 then Upcase (Argument (2)) else "*");
   Wildcard : constant Boolean := Has (Rack, '*');
   Letters  : constant String := Sort (Get_Letters (Rack & Pattern));
   Blanks   : constant Natural := Count (Rack, ' ') + Count (Rack, '?');
   Max_Len  : constant Natural := Letters'Length + Blanks;

   pragma Debug (Text_IO.Put_Line ("Rack = " & Rack));
   pragma Debug (Text_IO.Put_Line ("Wildcard = " & Wildcard'Img));
   pragma Debug (Text_IO.Put_Line ("Letters = " & Letters));
   pragma Debug (Text_IO.Put_Line ("Blanks =" & Blanks'Img));
   pragma Debug (Text_IO.Put_Line ("Pattern = " & Pattern));

   use Text_IO;

   function Read_Board (File : File_Type; Rows : Positive) return String is
     (if Rows = 1 then Left (Get_Line (File), Board_Size)
      else Left (Get_Line (File), Board_Size) & Read_Board (File, Rows - 1));

   procedure Analyze_Board (Words : String; Board : Board_String) is
      pragma Debug (Put_Line ("letter=" & Get_Letters (Board)));
      Used : String := Sort (Get_Letters (Upcase (Board)));
   begin
      Put_Line ("Tiles:     " & Tiles);
      Put_Line ("Used:      " & Used);
      Put_Line ("Remaining: " & Tiles - Used);

      if not Match_Rack (Used, Tiles) then
         Put_Line ("illegal board setup");
         raise Error_Exit;
      end if;
   end Analyze_Board;

   function "/" (Left : String; Right : Character) return Natural is
      Count : Natural := 0;
   begin
      for X of Left loop
         Count := Count + Boolean'Pos (X = Right);
      end loop;

      return Count;
   end "/";

   procedure Analyze_Dict (Dict : String) is
      Spaces  : constant Natural := Dict / ' ';
      Words   : constant Positive := 1 + Spaces;
      Letters : constant Natural := Dict'Length - Spaces;
      Count   : Natural;
      Percent : Natural;
   begin
      Put_Line (Words'Img & " words");
      for L in Letter loop
         Count := Dict / L;
         Percent := 100 * Count / Letters;
         Put_Line (Count'Img & " * " & L'Img & "," & Percent'Img & "%");
      end loop;
   end Analyze_Dict;

   function Read (File : File_Type; Lines : Natural) return String is
     (if Lines < 1 or else End_Of_File (File) then ""
      elsif Lines = 1 then Get_Line (File)
      else Read (File, Lines / 2) & ' ' & Read (File, Lines - Lines / 2));

   function Read (File : File_Type) return String is
     (Read (File, Natural'Last));

   function Skip (S : String; C : Character) return Positive is
     (if Empty (S) or else Head (S) /= C then S'First 
      else Skip (S (S'First + 1 .. S'Last), C));
   pragma Assert (Skip ("Hello", 'H') = 2);
   pragma Assert (Skip ("Hello", 'e') = 1);
   Pragma Assert (Skip ("ssslow", 's') = 4);
   pragma Assert (Skip ("***", '*') = 4);
   pragma Assert (Skip ("", ' ') = 1);

   function Skip_To (S : String; C : Character) return Positive is
     (if S = "" or else Head (S) = C then S'First else Skip_To (Tail (S), C));

   function Backup_Past (S : String; C : Character) return Natural is
     (if S = "" then S'First - 1
      elsif S (S'Last) = C then S'Last - 1
      else Backup_Past (S (S'First .. S'Last - 1), C));

   function Filter (Word : String) return Boolean is
     (if Wildcard then Match (Word, Rack)
      else Word'Length <= Max_Len
              and then Match_Rack (Sort (Word), Letters, Blanks)
              and then Match (Word, Pattern));

   procedure Usage is
   begin
      Put_Line ("Usage:");
      Put_Line (Command_Name & " [rack] pattern");
      Put_Line (Command_Name & " --board");
   end Usage;

   Word_File : File_Type;

begin
   begin
      Open (Word_File, In_File, "words.txt");
   exception
      when others =>
         Put_Line (Standard_Error, "error reading words.txt");
         raise Error_Exit;
   end;

   if Argument_Count = 0 then
      Usage;

   elsif Argument (1) = "--board" then
      Analyze_Board ("", Read_Board (Current_input, Board_Size));
      return;

   elsif Argument (1) = "--dict" then
      Analyze_Dict (Read (Word_File));
      return;
   end if;

   while not End_Of_File (Word_File) loop
      declare
         Word : constant String := Upcase(Get_Line (Word_File));
      begin
         if Filter (Word) then
            Put_Line (Word & Score (Word)'Img);
         end if;
      end;
   end loop;
exception
   when Error_Exit =>
      Set_Exit_Status (Failure);
end Anagram;
