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
      To_Load            : Boolean;
      Has_Stack          : Boolean;
      Init_Code          : Algorithm;
      Additional_Content : Unbounded_String_Vectors.Vector;
   end record;

   function Make_Section
      (Boot_Memory        : Unbounded_String;
       Name               : Unbounded_String;
       Reloc_Memory       : Unbounded_String;
       Init_Code          : Algorithm := No_Code;
       Force_Init         : Boolean := False;
       Load               : Boolean := True;
       Has_Stack          : Boolean := False;
       Additional_Content : Unbounded_String_Vectors.Vector :=
         Unbounded_String_Vectors.Empty_Vector)
       return Section;

   package Section_Vectors is new Ada.Containers.Vectors
      (Positive, Sections.Section);

end Sections;
