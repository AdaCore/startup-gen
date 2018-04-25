with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--------------
-- Sections --
--------------

package Sections is

  package Unbounded_String_Vect is new Ada.Containers.Vectors
      (Positive, Unbounded_String);

   type Section is tagged record
      Name : Unbounded_String;
      Reloc_Memory : Unbounded_String;
      To_Init : Boolean;
      Additional_Content : Unbounded_String_Vect.Vector;
   end record;

   function Make_Section
      (Boot_Memory : Unbounded_String;
       Name : Unbounded_String;
       Reloc_Memory : Unbounded_String;
       Force_Init : Boolean := False)
       return Section;

package Sect_Vect is new Ada.Containers.Vectors
      (Element_Type => Sections.Section,
       Index_Type   => Positive);

end Sections;
