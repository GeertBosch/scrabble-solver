pragma Ada_2012;
package Textfun is

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

   function Head (S : String) return Character is (S (S'First));
   --  Note: S must be non-empty

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
     (if S'Last < S'First then S
      elsif  S'Last = S'First then (S'First => Upcase (S (S'First)))
      else Upcase (Left (S)) & Upcase (Right (S)));
   pragma Assert (Upcase ("Hello, World!") = "HELLO, WORLD!");

   function Count (S : String; C : Character) return Natural is
     (if S'Last < S'First then 0
      else Boolean'Pos (Head (S) = C) + Count (Tail (S), C));

   function Has (S : String; C : Character) return Boolean is
      (not Empty (S) and then (Head (S) = C or else Has (Tail (S), C)));
   pragma Assert (Has ("*sha*mater", '*'));

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

end Textfun;
