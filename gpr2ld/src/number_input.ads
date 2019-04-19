with Interfaces;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Number_Input is

   function Valid (Str : String) return Boolean;

   function Valid (Str : Unbounded_String) return Boolean
   is (Valid (To_String (Str)));

   function Convert (Str : String) return Interfaces.Unsigned_64
     with Pre => Valid (Str);

   function Convert (Str : Unbounded_String) return Interfaces.Unsigned_64
   is (Convert (To_String (Str)))
     with Pre => Valid (Str);

   function To_C_Hexadecimal (Val : Interfaces.Unsigned_64) return String;
   --  Convert to a string representation in C literal format

end Number_Input;
