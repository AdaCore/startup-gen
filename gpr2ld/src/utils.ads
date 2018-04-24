with GNATCOLL.Projects; use GNATCOLL.Projects;

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-----------
-- Utils --
-----------

package Utils is

   package UString_Vect is new Ada.Containers.Vectors (Positive,
      Unbounded_String);

   ------------------------------------
   -- Register_Memory_Map_Attributes --
   ------------------------------------

   procedure Register_Memory_Map_Attributes;

end Utils;
