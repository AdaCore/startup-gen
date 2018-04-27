with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

with GNATCOLL.Templates; use GNATCOLL.Templates;

-------------
-- Startup --
-------------

package Startup is

   type Algorithm is tagged private;

   Clear_Memory_Code : constant Algorithm;
   Copy_Memory_Code  : constant Algorithm;
   No_Code           : constant Algorithm;

   procedure Format_Code
      (Self : in out Algorithm;
       Subst : Substitution_Value);

   procedure Format_Code_With_Name
      (Self : in out Algorithm;
       Name : Unbounded_String);

   procedure Format_Code_With_Indent
      (Self : in out Algorithm;
       Indent : Unbounded_String);

private

   package Unbounded_String_Vector is new Ada.Containers.Vectors
      (Positive, Unbounded_String);
   use Unbounded_String_Vector;

   package SV renames Unbounded_String_Vector;

   type Algorithm is tagged record
      Code : Unbounded_String_Vector.Vector;
   end record;

   function TUS (Str : String) return Unbounded_String
      renames To_Unbounded_String;

   --  Private constants  --

   Clear_Memory_Code : constant Algorithm :=
      (Code => SV.Empty_Vector &
            TUS ("/* Clear $NAME */") &
            TUS ("$INDENTmovw  r0,#:lower16:__$NAME_start") &
            TUS ("$INDENTmovw  r0,#:uppper16:__$NAME_start") &
            TUS ("$INDENTmovw  r1,#:lower16:__$NAME_words") &
            TUS ("$INDENTmovw  r2,#0") &
            TUS ("$INDENTmovw  r1,1f") &
            TUS ("$INDENTcbz  r1,1f") &
            TUS ("0:$INDENTstr  r2,[r0],#4") &
            TUS ("$INDENTsubs  r1,r1,#1") &
            TUS ("$INDENTbne  0b") &
            TUS ("") &
            TUS ("1:")
         );

      Copy_Memory_Code : constant Algorithm :=
         (Code => SV.Empty_Vector &
               TUS ("/* Copy $NAME */") &
               TUS ("$INDENTmovw  r0,#:lower16:__$NAME_start") &
               TUS ("$INDENTmovw  r0,#:uppper16:__$NAME_start") &
               TUS ("$INDENTmovw  r1,#:lower16:__$NAME_words") &
               TUS ("$INDENTmovw  r2,#:lower16:__$NAME_load") &
               TUS ("$INDENTmovw  r2,#:uppper16:__$NAME_load") &
               TUS ("$INDENTcbz  r1,1f") &
               TUS ("0:$INDENTldr  r4,[r2],#4") &
               TUS ("$INDENTstr  r4,[r0],#4") &
               TUS ("$INDENTsubs  r1,r1,#1") &
               TUS ("$INDENTbne  0b") &
               TUS ("") &
               TUS ("1:")
            );

      No_Code : constant Algorithm := (Code => SV.Empty_Vector);

end Startup;
