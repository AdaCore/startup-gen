with GNATCOLL.Projects; use GNATCOLL.Projects;

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Utils is

   package UString_Vect is new
           Ada.Containers.Vectors (Positive, Unbounded_String);

   procedure Register_Memory_Map_Attributes with
      Post => (Attribute_Registered ("Mem_Kind", "Memory")
               and Attribute_Registered ("Size", "Memory")
               and Attribute_Registered ("Start", "Memory")
               and Attribute_Registered ("Memories", "Memory")
               );
end Utils;
