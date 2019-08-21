------------------------------------------------------------------------------
--                                                                          --
--                               startup-gen                                --
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

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
