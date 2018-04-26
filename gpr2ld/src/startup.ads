with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

with GNATCOLL.Templates; use GNATCOLL.Templates;

-------------
-- Startup --
-------------

package Startup is

   type Algorithm is tagged private;

   function Clear_Memory_Code return Algorithm;

   function Copy_Memory_Code return Algorithm;

   function No_Code return Algorithm;

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
      Load : Boolean;
   end record;

   function TUS (Str : String) return Unbounded_String
      renames To_Unbounded_String;

end Startup;
