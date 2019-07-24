with Ada.Text_IO;

with Interfaces;            use Interfaces;
with GNAT.Regpat;           use GNAT.Regpat;
with GNATCOLL.Utils;        use GNATCOLL.Utils;
with Utils;                 use Utils;

package body Number_Input is

   package U64_IO is new Ada.Text_IO.Modular_IO (Unsigned_64);

   Dec_Number_And_Unit : constant String :=
     "(^[0-9]+(k|K|m|M)$)";

   Hex_Number : constant String :=
     "(^0x([0-9]|[A-F]|[a-f])+$)";

   Pattern : constant String :=
     Hex_Number & "|" & Dec_Number_And_Unit;

   Matcher : constant Pattern_Matcher := Compile (Pattern);

   -----------
   -- Valid --
   -----------

   function Valid (Str : String) return Boolean is
   begin
      if Match (Matcher, Str) then
         return True;
      else

         --  Check if Str is a valid Ada literal by trying to do a conversion
         declare
            U : Unsigned_64;
            pragma Unreferenced (U);
         begin
            U := Unsigned_64'Value (Str);
         exception
            when others => return False;
         end;
         return True;
      end if;
   end Valid;

   -------------
   -- Convert --
   -------------

   function Convert (Str : String) return Unsigned_64 is
      Multiplier : Unsigned_64 := 1;
   begin
      if Match (Matcher, Str) then
         if Starts_With (Str, "0x") then
            return Unsigned_64'Value
              ("16#" & Str (Str'First + 2 .. Str'Last) & "#");
         else
            case Str (Str'Last) is
               when 'k' | 'K' =>
                  Multiplier := 1024;
               when 'm' | 'M' =>
                  Multiplier := 1024 * 1024;
               when others =>
                  Fatal_Error
                    ("Invalid input to number conversion: '" & Str & "'");
            end case;
         end if;
         return Unsigned_64'Value
           (Str (Str'First .. Str'Last - 1)) * Multiplier;
      else
         return Unsigned_64'Value (Str);
      end if;
   end Convert;

   ----------------------
   -- To_C_Hexadecimal --
   ----------------------

   function To_C_Hexadecimal (Val : Interfaces.Unsigned_64) return String is
      Temp_String : String (1 .. 256);
      Result      : Unbounded_String;
   begin
      U64_IO.Put (To   => Temp_String,
                  Item => Val,
                  Base => 16);

      Result := To_Unbounded_String (Temp_String);
      Trim (Result, Ada.Strings.Left);
      return ("0x" & Slice (Result, 4, Length (Result) - 1));
   end To_C_Hexadecimal;


end Number_Input;
