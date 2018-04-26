with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Startup; use Startup;

--------------
-- Sections --
--------------

package Sections is

   package Unbounded_String_Vectors is new Ada.Containers.Vectors
      (Positive, Unbounded_String);

   type Section is tagged record
      Name               : Unbounded_String;
      Reloc_Memory       : Unbounded_String;
      To_Init            : Boolean;
      Init_Code          : Algorithm;
      Additional_Content : Unbounded_String_Vectors.Vector;
   end record;

   function Make_Section
      (Boot_Memory  : Unbounded_String;
       Name         : Unbounded_String;
       Reloc_Memory : Unbounded_String;
       Init_Code    : Algorithm := No_Code;
       Force_Init   : Boolean := False)
       return Section;

   package Section_Vectors is new Ada.Containers.Vectors
      (Positive, Sections.Section);

end Sections;
