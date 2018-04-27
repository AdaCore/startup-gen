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

   package Unbounded_String_Vector is new Ada.Containers.Vectors
      (Positive, Unbounded_String);

   function Get_Lines (Self : in out Algorithm)
      return Unbounded_String_Vector.Vector;

   procedure Format
      (Self   : in out Algorithm;
       Name   : Unbounded_String;
       Indent : Unbounded_String);

private

   procedure Format_Code
      (Self : in out Algorithm;
       Subst : Substitution_Value);

   procedure Format_Code_With_Name
      (Self : in out Algorithm;
       Name : Unbounded_String);

   procedure Format_Code_With_Indent
      (Self : in out Algorithm;
       Indent : Unbounded_String);

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
            TUS ("/* Clear ${NAME} */") &
            TUS ("${INDENT}movw  r0,#:lower16:__${NAME}_start") &
            TUS ("${INDENT}movw  r0,#:uppper16:__${NAME}_start") &
            TUS ("${INDENT}movw  r1,#:lower16:__${NAME}_words") &
            TUS ("${INDENT}movw  r2,#0") &
            TUS ("${INDENT}movw  r1,1f") &
            TUS ("${INDENT}cbz  r1,1f") &
            TUS ("0:${INDENT}str  r2,[r0],#4") &
            TUS ("${INDENT}subs  r1,r1,#1") &
            TUS ("${INDENT}bne  0b") &
            TUS ("") &
            TUS ("1:")
         );

      Copy_Memory_Code : constant Algorithm :=
         (Code => SV.Empty_Vector &
               TUS ("/* Copy ${NAME} */") &
               TUS ("${INDENT}movw  r0,#:lower16:__${NAME}_start") &
               TUS ("${INDENT}movw  r0,#:uppper16:__${NAME}_start") &
               TUS ("${INDENT}movw  r1,#:lower16:__${NAME}_words") &
               TUS ("${INDENT}movw  r2,#:lower16:__${NAME}_load") &
               TUS ("${INDENT}movw  r2,#:uppper16:__${NAME}_load") &
               TUS ("${INDENT}cbz  r1,1f") &
               TUS ("0:${INDENT}ldr  r4,[r2],#4") &
               TUS ("${INDENT}str  r4,[r0],#4") &
               TUS ("${INDENT}subs  r1,r1,#1") &
               TUS ("${INDENT}bne  0b") &
               TUS ("") &
               TUS ("1:")
            );

      No_Code : constant Algorithm := (Code => SV.Empty_Vector);

end Startup;
